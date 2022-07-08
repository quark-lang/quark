{-# LANGUAGE FlexibleInstances #-}
module Core.Inference.Type where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Control.Monad.RWS
    ( MonadIO (liftIO),
      RWST(runRWST),
      MonadReader(local, ask), when, void )
  import Data.Foldable (foldlM)
  import Control.Monad (unless)
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty
  import Core.Inference.Type.Methods
  import Core.Inference.Type.Parsing
  import System.Exit
  import Data.Maybe (fromMaybe)
  import Core.Utility.Color (red, bBlack, bold)
  import Control.Monad.Except (runExceptT, MonadError (throwError))
  import Data.Either (fromLeft)
  
  tyPattern :: MonadType m => A.Expression -> m (TypedPattern, SubTy, Type, M.Map String Type)
  tyPattern (A.Identifier "_") = do
    t <- tyFresh
    return (WilP t, M.empty, t, M.singleton "_" t)
  tyPattern (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarP n t', M.empty, t', M.empty)
    Nothing -> do
      t <- tyFresh
      return (VarP n t, M.empty, t, M.singleton n t)
  tyPattern z@(A.Node e@(A.Identifier n) xs) = do
    tv <- tyFresh
    (n', s1, t1, m1) <- tyPattern e
    (x', s2, t2, m2) <- foldlM (\(x', s, t, m) x -> do
      (x'', s', t', m') <- local (applyTypes (tyApply s)) $ tyPattern x
      return (x' ++ [x''], s `tyCompose` s', t ++ [t'], m' `M.union` m)) ([], s1, [], M.empty) xs
    case tyUnify (tyApply s2 t1) (t2 :-> tv) of
      Right s3 -> do
        let x'' = tyApply s3 tv
        return (AppP n x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'', m1 `M.union` m2)
      Left x -> throwError (x, z)

  tyPattern (A.Literal (A.String s)) = return (LitP (S s) String, M.empty, String, M.empty)
  tyPattern (A.Literal (A.Integer i)) = return (LitP (I i) Int, M.empty, Int, M.empty)
  tyPattern (A.Literal (A.Float f)) = return (LitP (F f) Float, M.empty, Float, M.empty)
  tyPattern _ = error "tyPattern: not implemented"

  -- Main type inference function
  tyInfer :: MonadType m => A.Expression -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> throwError ("Variable " ++ bold n ++ " is not defined.", A.Identifier n)

  tyInfer z@(A.Node (A.Identifier "if") [cond, then_, else_]) = do
    (cond', s1, cond_t) <- tyInfer cond
    (then_', s2, then_t) <- local (applyTypes (tyApply s1)) $ tyInfer then_
    (else_', s3, else_t) <- local (applyTypes (tyApply s2)) $ tyInfer else_
    case tyUnify cond_t Bool of
      Right s' -> do
        let s4 = s' `tyCompose` s1 `tyCompose` s2 `tyCompose` s3
        unless (tyApply s4 then_t == tyApply s4 else_t) $
          throwError ("Type " ++ show then_t ++ " does not match type " ++ show else_t, z)
        return (tyApply s4 $ IfE cond' then_' else_', s4, tyApply s4 then_t)
      Left x -> throwError (x, z)

  tyInfer z@(A.Node (A.Identifier "match") (pat:cases)) = do
    (pat', s1, pat_t) <- tyInfer pat

    (xs', s2) <- foldlM (\(acc, s') l@(A.List [pattern, body]) -> do
                  (pattern', s'', pattern_t, bound) <- local
                                                        (applyTypes (tyApply s'))
                                                        (tyPattern pattern)
                  case tyUnify pattern_t (tyApply s'' pat_t) of
                    Right s''' -> do
                      let s2 =  s''' `tyCompose` s''
                      let pattern2   = tyApply s2 pattern'
                      let bound' = M.map (Forall [] . tyApply s2) bound
                      (body', s3, body_t) <- local (applyTypes (\e -> tyApply s2 (e `M.union` bound'))) $ tyInfer body

                      let body2 = tyApply s3 body'
                      let body_t2 = tyApply s3 body_t

                      let pattern3  = tyApply s3 pattern2
                      unless (null acc) $ do
                        let (_, _, t) = last acc
                        let s4 = tyUnify t body_t2
                        case s4 of
                          Left err -> throwError (err, l)
                          Right _ -> return ()
                      return (acc ++ [(pattern3, body2, body_t2)], s2 `tyCompose` s3)
                    Left x -> throwError (x, z)
                  ) ([], s1) cases

    let cases' = map (\(p, t, _) -> (p, t)) xs'
    let t' = map (\(_, _, t) -> t) xs'
    let s3 = s2 `tyCompose` s1
    return (PatternE pat' cases', s3, last t')

  -- Type inference for abstractions
  tyInfer (A.Node (A.Identifier "fn") [A.List args, body]) = do
    tvs <- mapM (const tyFresh) args
    let args' = map unliteral args
    Env env _ <- ask
    let env'  = foldl (flip M.delete) env args'
        env'' = env' `M.union` M.fromList [(x, Forall [] tv) | (x, tv) <- zip args' tvs]

    (b', s1, t1) <- local (applyTypes (const env'')) $ tyInfer body
    let argTy = tyApply s1 tvs

    return (AbsE (zip args' argTy) b', s1, argTy :-> t1)

  -- Type inference for let-polymorphic expressions
  tyInfer z@(A.Node (A.Identifier "let") [A.Identifier name, value, body]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value

    Env env _ <- ask

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Right s -> do
                let r = tyApply s t1
                unless (r == t'') $
                  throwError ("Type " ++ show r ++ " does not match type " ++ show t'', z)
                return (s `tyCompose` s1, r)
              Left x -> throwError (x, z)
          Nothing -> return (s1, t1)
    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) t2
        env'' = M.insert name t' env'

    (b', s2, t2) <- local (applyTypes . const $ tyApply s1 env'') $ tyInfer body
    return (LetInE (name, t1) (tyApply s3 v') b', s3 `tyCompose` s2, t2)

  tyInfer (A.List elems) = do
    (elems', s, t) <- foldlM (\(es, s, t) e -> do
      (e', s', t') <- tyInfer e
      return (es ++ [e'], s `tyCompose` s', t ++ [t'])) ([], M.empty, []) elems

    let initType = head t
    unless (all (==initType) t) $
      let error = "Type mismatch with " ++ show initType ++ " and " ++ show (head $ filter (/=initType) t)
        in throwError (error, A.List elems)

    return (ListE elems' (head t), s, ListT (head t))

  -- Type inference errors
  tyInfer z@(A.Node (A.Identifier "let") _) 
    = throwError ("Cannot have let as last expression in block", z)

  tyInfer z@(A.Node (A.Identifier "data") _) 
    = throwError ("Cannot have nested data constructor", z)

  tyInfer z@(A.Node (A.Identifier "declare") _) 
    = throwError ("Cannot have nested declaration", z)

  -- Type inference for applications
  tyInfer z@(A.Node n xs) = do
    tv <- tyFresh
    (n', s1, t1) <- tyInfer n
    (x', s2, t2) <- foldlM (\(x', s, t) x -> do
      (x'', s', t') <- local (applyTypes (tyApply s)) $ tyInfer x
      return (x' ++ [x''], s `tyCompose` s', t ++ [t'])) ([], s1, []) xs
    case tyUnify (t2 :-> tv) (tyApply s2 t1) of
      Right s3 -> do
        let x'' = tyApply s3 tv
        return (AppE n' x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'')
      Left x -> throwError (x, z)

  -- Value related inference
  tyInfer (A.Literal (A.String s))  = return (LitE (S s) String, M.empty, String)
  tyInfer (A.Literal (A.Integer i)) = return (LitE (I i) Int, M.empty, Int)
  tyInfer (A.Literal (A.Float f))   = return (LitE (F f) Float, M.empty, Float)
  tyInfer a = throwError ("Unknown expression", a)

  topLevel :: MonadType m => A.Expression -> m (Maybe [TypedAST], Env)
  -- Empty data constructor (just a phantom type)
  topLevel (A.Node (A.Identifier "data") [dat]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) (A.List [])
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level data with constructors
  topLevel (A.Node (A.Identifier "data") [dat, constructors]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) constructors
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level let expression (let-in shouldn't be used in top-level)
  topLevel z@(A.Node (A.Identifier "let") [A.Identifier name, value]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value

    Env env c <- ask

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Right s -> do
                let r = tyApply s t''
                return (s `tyCompose` s1, r)
              Left x -> throwError (x, z)
          Nothing -> return (s1, t1)

    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) t2
        env'' = M.insert name t' env'
    return (Just [LetE (name, t2) (tyApply s3 v')], Env env'' c)

  -- Top-level declare used to define function type
  topLevel z@(A.Node (A.Identifier "declare") [dat, def]) = do
    let (name, tyArgs) = parseTypeHeader dat

    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs

    let ty = parseType argsMap def

    e@(Env t _) <- ask
    --kindsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    --((_, b), _, _) <- runRWST (kyCheck def) (k `M.union` M.map (\(TVar k) ---> KVar k) kindsMap) 0

    --liftIO $ print (b, name)
    return (Nothing, applyTypes (`M.union` M.singleton name (generalize t ty)) e)
  topLevel x = throwError ("Unknown top-level expression received", x)

  functions :: TypeEnv
  functions = M.fromList [
      ("*", Forall [] $ [Int, Int] :-> Int),
      ("-", Forall [] $ [Int, Int] :-> Int)
    ]

  runInfer :: MonadIO m => [A.Expression] -> m (Either (String, A.Expression) [TypedAST])
  runInfer a = do
    let e = Env functions M.empty
    fmap fst <$> foldlM (\e x -> case e of
      Right (a, e) -> do
        x <- runExceptT $ runRWST (topLevel x) e 0
        case x of
          Right ((a', e'), _, _) -> return $ Right (case a' of
            Nothing -> a
            Just a' -> a ++ a', mergeEnv e e')
          Left err -> return $ Left err
      Left err -> return $ Left err) (Right ([], e)) a
