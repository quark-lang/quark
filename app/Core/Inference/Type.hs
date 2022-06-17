{-# LANGUAGE FlexibleInstances, BangPatterns #-}
module Core.Inference.Type where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Control.Monad.RWS
    ( MonadIO (liftIO),
      RWST(runRWST),
      MonadReader(local, ask) )
  import Data.Foldable (foldlM)
  import Control.Monad (unless)
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty
  import Core.Inference.Type.Methods
  import Core.Inference.Type.Parsing
  import System.Exit

  tyPattern :: MonadType m => A.AST -> m (TypedPattern, SubTy, Type, M.Map String Type)
  tyPattern (A.Literal "_") = do
    t <- tyFresh
    return (WilP t, M.empty, t, M.singleton "_" t)
  tyPattern (A.Literal n) = ask >>= \(Env t c _) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarP n t', M.empty, t', M.empty)
    Nothing -> do
      t <- tyFresh
      return (VarP n t, M.empty, t, M.singleton n t)
  tyPattern z@(A.Node n [x]) = do
    tv <- tyFresh
    (n', s1, t1, m1) <- tyPattern n
    (x', s2, t2, m2) <- local (applyTypes (tyApply s1)) $ tyPattern x
    case tyUnify (tyApply s2 t1) (t2 :-> tv) of
      Left s3 -> do
        let x'' = tyApply s3 tv
        return (AppP n' x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'', m1 `M.union` m2)
      Right x -> liftIO $ do
         mapM putStrLn x
         print z
         exitFailure

  tyPattern (A.String s) = return (LitP (S s) String, M.empty, String, M.empty)
  tyPattern (A.Integer i) = return (LitP (I i) Int, M.empty, Int, M.empty)
  tyPattern (A.Float f) = return (LitP (F f) Float, M.empty, Float, M.empty)

  -- Main type inference function
  tyInfer :: MonadType m => A.AST -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Literal n) = ask >>= \(Env t c _) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> error $ "Variable " ++ show n ++ " is not defined."

  tyInfer z@(A.Node (A.Literal "if") [cond, then_, else_]) = do
    (cond', s1, cond_t) <- tyInfer cond
    (then_', s2, then_t) <- local (applyTypes (tyApply s1)) $ tyInfer then_
    (else_', s3, else_t) <- local (applyTypes (tyApply s2)) $ tyInfer else_
    case tyUnify cond_t Bool of
      Left s' -> do
        let s4 = s' `tyCompose` s1 `tyCompose` s2 `tyCompose` s3
        return (tyApply s4 $ IfE cond' then_' else_', s4, then_t)
      Right x -> liftIO $ do
         mapM putStrLn x
         print z
         exitFailure

  tyInfer z@(A.Node (A.Literal "match") (pat:cases)) = do
    (pat', s1, pat_t) <- tyInfer pat

    (xs', s2) <- foldlM (\(acc, s') (A.List [pattern, body]) -> do
                  (pattern', s'', pattern_t, bound) <- local
                                                        (applyTypes (tyApply s'))
                                                        (tyPattern pattern)
                  case tyUnify (tyApply s'' pat_t) pattern_t of
                    Left s''' -> do
                      let s2 =  s''' `tyCompose` s''
                      let pattern2   = tyApply s2 pattern'
                      let bound' = M.map (Forall [] . tyApply s2) bound

                      (body', s3, body_t) <- local (applyTypes (\e -> tyApply s2 (e `M.union` bound'))) $ tyInfer body

                      let body2 = tyApply s3 body'
                      let body_t2 = tyApply s3 body_t

                      let pattern3  = tyApply s3 pattern'

                      return (acc ++ [(pattern3, body2, body_t2)], s3 `tyCompose` s2)
                    Right x -> liftIO $ do
                       mapM putStrLn x
                       print z
                       exitFailure
                  ) ([], s1) cases

    let cases' = map (\(p, t, _) -> (p, t)) xs'
    let t' = map (\(_, _, t) -> t) xs'
    let s3 = s2 `tyCompose` s1
    return $ (PatternE pat' cases', s3, last t')

  -- Type inference for abstractions
  tyInfer (A.Node (A.Literal "fn") [A.Literal arg, body]) = do
    tv <- tyFresh

    Env env _ _ <- ask
    let env'  = M.delete arg env
        env'' = env' `M.union` M.singleton arg (Forall [] tv)

    (b', s1, t1) <- local (applyTypes (const env'')) $ tyInfer body
    let argTy = tyApply s1 tv

    return (AbsE (arg, argTy) b', s1, argTy :-> t1)

  -- Type inference for let-polymorphic expressions
  tyInfer z@(A.Node (A.Literal "let") [A.Literal name, value, body]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value

    Env env _ _ <- ask

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Left s -> do
                let r = tyApply s t1
                unless (r == t'') $ error $ "Type " ++ show r ++ " does not match type " ++ show t''
                return (s `tyCompose` s1, r)
              Right x -> liftIO $ do
                 mapM putStrLn x
                 print z
                 exitFailure
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
      error $ "Type mismatch with " ++ show initType ++ " and " ++ show (head $ filter (/=initType) t)

    return (ListE elems' (head t), s, ListT (head t))

  -- Type inference for applications
  tyInfer z@(A.Node n [x]) = do
    tv <- tyFresh
    (n', s1, t1) <- tyInfer n
    (x', s2, t2) <- local (applyTypes (tyApply s1)) $ tyInfer x
    case tyUnify (tyApply s2 t1) (t2 :-> tv) of
      Left s3 -> do
        let x'' = tyApply s3 tv
        return (AppE n' x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'')
      Right x -> liftIO $ do
         mapM putStrLn x
         print z
         exitFailure

  -- Value related inference
  tyInfer (A.String s)  = return (LitE (S s) String, M.empty, String)
  tyInfer (A.Integer i) = return (LitE (I i) Int, M.empty, Int)
  tyInfer (A.Float f)   = return (LitE (F f) Float, M.empty, Float)
  tyInfer a = error $ "AST node not supported: " ++ show a

  topLevel :: MonadType m => A.AST -> m (Maybe [TypedAST], Env)
  -- Empty data constructor (just a phantom type)
  topLevel (A.Node (A.Literal "data") [dat]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) (A.List [])
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level data with constructors
  topLevel (A.Node (A.Literal "data") [dat, constructors]) = do
    (constr', ast) <- parseData (parseTypeHeader dat) constructors
    e <- ask
    return (Just [ast], applyCons (`M.union` constr') e)

  -- Top-level let expression (let-in shouldn't be used in top-level)
  topLevel z@(A.Node (A.Literal "let") [A.Literal name, value]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value

    Env env c k <- ask

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            case tyUnify t1 t'' of
              Left s -> do
                let r = tyApply s t''
                return (s `tyCompose` s1, r)
              Right x -> liftIO $ do
                mapM putStrLn x
                print z
                exitFailure
          Nothing -> return (s1, t1)

    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) t2
        env'' = M.insert name t' env'
    return (Just [LetE (name, t2) (tyApply s3 v')], Env env'' c k)

  -- Top-level declare used to define function type
  topLevel z@(A.Node (A.Literal "declare") [dat, def]) = do
    let (name, tyArgs) = parseTypeHeader dat

    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs

    let ty = parseType argsMap def

    e@(Env t _ k) <- ask
    --kindsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    --((_, b), _, _) <- runRWST (kyCheck def) (k `M.union` M.map (\(TVar k) ---> KVar k) kindsMap) 0

    --liftIO $ print (b, name)
    case ty of
      Left k -> return (Nothing, applyKinds (`M.union` M.singleton name k) e)
      Right t' -> return (Nothing, applyTypes (`M.union` M.singleton name (generalize t t')) e)
  topLevel x = error $ "Invalid top level expression, received: " ++ show x

  runInfer :: MonadIO m => A.AST -> m [TypedAST]
  runInfer a = do
    let e = emptyEnv
    (a', env) <- case a of
      A.Node (A.Literal "begin") [A.List xs] ->
        foldlM (\(a, e) x -> do
          ((a', e'), _, _) <- runRWST (topLevel x) e 0
          return (case a' of
            Nothing -> a
            Just ast -> a ++ ast, mergeEnv e e')) ([], e) xs
      x -> do
        ((a', e), _, _) <- runRWST (topLevel x) e 0
        return (case a' of
          Nothing -> []
          Just ast -> ast, e)
    return a'
