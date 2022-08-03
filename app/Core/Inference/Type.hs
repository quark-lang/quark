{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Core.Inference.Type where
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Control.Monad.RWS
    ( MonadIO (liftIO),
      RWST(runRWST),
      MonadReader(local, ask), when, void, forM )
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
  import Data.Either (fromLeft, isLeft)
  import GHC.Float (double2Float)
  import Debug.Trace (traceShow)
  
  tyPattern :: MonadType m => A.Expression -> m (TypedPattern, SubTy, Type, M.Map String Scheme)
  tyPattern (A.Identifier "_") = do
    t <- tyFresh
    return (WilP t, M.empty, t, M.empty)
  tyPattern (A.Identifier "true") = return (VarP "true" Bool, M.empty, Bool, M.empty)
  tyPattern (A.Identifier "false") = return (VarP "false" Bool, M.empty, Bool, M.empty)
  tyPattern (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n c of
    Just t -> do
      t' <- tyInstantiate t
      return (VarP n t', M.empty, t', M.empty)
    Nothing -> do
      t <- tyFresh
      return (VarP n t, M.empty, t, M.singleton n (Forall [] t))
  tyPattern z@(A.Node e@(A.Identifier n) xs) = do
    tv <- tyFresh
    (n', s1, t1, m1) <- tyPattern e
    (x', s2, t2, m2) <- foldlM (\(x', s, t, m) x -> do
      (x'', s', t', m') <- local (applyTypes (tyApply s)) $ tyPattern x
      return (x' ++ [x''], s `tyCompose` s', t ++ [t'], m' `M.union` m)) ([], s1, [], M.empty) xs
    case tyUnify (t2 :-> tv) (tyApply s2 t1)  of
      Right s3 -> do
        let x'' = tyApply s3 tv
        return (AppP n x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'', m1 `M.union` m2)
      Left x -> throwError (x, z)

  tyPattern (A.Literal (A.String s)) = return (LitP (S s) String, M.empty, String, M.empty)
  tyPattern (A.Literal (A.Integer i)) = return (LitP (I i) Int, M.empty, Int, M.empty)
  tyPattern (A.Literal (A.Float f)) = return (LitP (F f) Float, M.empty, Float, M.empty)
  tyPattern (A.Literal (A.Char c)) = return (LitP (C c) Char, M.empty, Char, M.empty)
  tyPattern x = error $ "tyPattern: not implemented => " ++ show x

  patUnify :: Type -> [Type] -> Either String SubTy
  patUnify x = foldl (\acc y -> tyCompose <$> tyUnify x y <*> acc) (Right M.empty)

  -- Main type inference function
  tyInfer :: MonadType m => A.Expression -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Identifier n) = ask >>= \(Env t c) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> throwError ("Variable " ++ bold n ++ " is not defined.", A.Identifier n)

  tyInfer z@(A.Node (A.Identifier "match") (pat:cases)) = do
    (pat', s1, pat_t) <- tyInfer pat

    let patterns = map (\(A.List [p, _]) -> p) cases
    let exprs    = map (\(A.List [_, c]) -> c) cases
    let cases'   = zip patterns exprs

    res <- forM cases' $ \(pattern, expr) -> do
      (p, s, t, m) <- tyPattern pattern
      let s2 = s `tyCompose` s1
      (e, s', t') <- local (applyTypes $ tyApply s2 . (m `M.union`)) $ tyInfer expr
      let s3 = s' `tyCompose` s2
      return (tyApply s3 t, tyApply s3 t', s3, (tyApply s3 p, tyApply s3 e))
    
    if null res 
      then (PatternE pat' [], M.empty,) <$> tyFresh
      else do
        let (_, t, _, _) = head res
        let s = foldl (\acc (tp, te, s, _) -> 
                let r = tyCompose <$> tyUnify t te <*> tyUnify tp pat_t
                    r' = tyCompose <$> r <*> acc
                  in tyCompose s <$> r') (Right s1) res
        let tys = map (\(x, _, _, _) -> case s of
                    Right s -> tyApply s x
                    Left _ -> x) res
        let s' = foldl (\acc x -> tyCompose <$> acc <*> patUnify x tys) (Right M.empty) tys
        case tyCompose <$> s <*> s' of
          Right s -> do
            let patterns' = map (\(_, _, _, (x, y)) -> (tyApply s x, tyApply s y)) res
            return (PatternE (tyApply s pat') patterns', s, tyApply s t)
          Left e -> throwError (e, z)
          
  -- Type inference for abstractions
  tyInfer (A.Node (A.Identifier "fn") [A.List args, body]) = do
    tvs <- mapM (const tyFresh) args
    let args' = map unliteral args
    Env env _ <- ask
    let env'  = foldl (flip M.delete) env args'
        env'' = env' `M.union` M.fromList [(x, Forall [] tv) | (x, tv) <- zip args' tvs]

    (b', s1, t1) <- local (applyTypes (const env'')) $ tyInfer body
    let argTy = tyApply s1 tvs
    return (tyApply s1 $ AbsE (zip args' argTy) b', s1, argTy :-> t1)

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
                let s' = s `tyCompose` s1
                let r = tyApply s' t1
                return (s', r)
              Left x -> throwError (x, z)
          Nothing -> return (s1, t1)
    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) (tyApply s3 t2)
        env'' = M.insert name t' env'

    (b', s2, t2) <- local (applyTypes . const $ tyApply s3 env'') $ tyInfer body
    let s4 = s2 `tyCompose` s3
    return (LetInE (name, tyApply s4 t1) (tyApply s4 v') (tyApply s4 b'), s4, tyApply s4 t2)

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
        return (AppE n' (tyApply s3 x') x'', s3 `tyCompose` s2 `tyCompose` s1, x'')
      Left x -> throwError (x, z)

  -- Value related inference
  tyInfer (A.Literal (A.String s))  = return (LitE (S s) String, M.empty, String)
  tyInfer (A.Literal (A.Integer i)) = return (LitE (I i) Int, M.empty, Int)
  tyInfer (A.Literal (A.Float f))   = return (LitE (F f) Float, M.empty, Float)
  tyInfer (A.Literal (A.Char c))    = return (LitE (C c) Char, M.empty, Char)
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
        t'    = generalize (tyApply s3 env) (tyApply s3 t2)
        env'' = M.insert name t' env'
    --liftIO $ print (name, tyApply s3 t2)
    return (Just [LetE (name, tyApply s3 t2) (tyApply s3 v')], Env env'' c)

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

  runInfer :: MonadIO m => [A.Expression] -> m (Either (String, A.Expression) [TypedAST])
  runInfer a =
    fmap fst <$> foldlM (\e x -> case e of
      Right (a, e) -> do
        x <- runExceptT $ runRWST (topLevel x) e 0
        case x of
          Right ((a', e'), _, _) -> return $ Right (case a' of
            Nothing -> a
            Just a' -> a ++ a', mergeEnv e e')
          Left err -> return $ Left err
      Left err -> return $ Left err) (Right ([], emptyEnv)) a
