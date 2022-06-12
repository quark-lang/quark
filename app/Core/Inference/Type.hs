{-# LANGUAGE FlexibleInstances #-}
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

  -- Import related
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)

  -- Pattern matching inference
  tyPattern :: MonadType m => (A.AST, [A.AST]) -> m (TypedAST, SubTy, Type)
  tyPattern (pat, cases) = do
    (pat', subTy, patType) <- tyInfer pat
    cases' <- mapM (`tyCase` patType) cases
    let s = foldl (\s (_, s', _) -> s `tyCompose` s') M.empty cases'
        c = map (\(pat, subTy, _) -> pat) cases'
        t = head $ map (\(_, _, t) -> t) cases'

    let cases'' = map (\(p, t) -> (tyApply s p, tyApply s t)) c
    return $ (PatternE (tyApply s pat') cases'', s, snd t)

  tyCase :: MonadType m => A.AST -> Type -> m ((TypedAST, TypedAST), SubTy, (Type, Type))
  tyCase (A.List [case', body]) t =
    case case' of
       A.Literal "_" -> do
         tv <- tyFresh
         (body, s1, t1) <- tyInfer body
         return ((VarE "_" t, body), s1, (tv, t1))
       A.Literal n -> do
         tv <- tyFresh
         let e = M.singleton n (Forall [] tv)
         (body, s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer body
         let s2 = tyUnify t (tyApply s1 tv)
         return ((VarE n t, body), s1 `tyCompose` s2, (tyApply s1 tv, t1))
       A.Integer n -> do
         (body, s1, t1) <- tyInfer body
         return ((LitE (A.Integer n) t, body), s1, (Int, t1))
       _ -> error . show $ case'

  -- Main type inference function
  tyInfer :: MonadType m => A.AST -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Literal n) = ask >>= \(Env t c _) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> error $ "Variable " ++ show n ++ " is not defined."

  tyInfer (A.Node (A.Literal "if") [cond, then_, else_]) = do
    (cond', s1, cond_t) <- tyInfer cond
    (then_', s2, then_t) <- local (applyTypes (tyApply s1)) $ tyInfer then_
    (else_', s3, else_t) <- local (applyTypes (tyApply s2)) $ tyInfer else_
    let s4 = s3 `tyCompose` s2 `tyCompose` s3
    return (tyApply s4 $ IfE cond' then_' else_', s4, then_t)

  tyInfer (A.Node (A.Literal "match") (pat:cases))
    = tyPattern (pat, cases)

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
  tyInfer (A.Node (A.Literal "let") [A.Literal name, value, body]) = do
    -- Fresh type for recursive definitions
    tv <- tyFresh
    let e =  M.singleton name (Forall [] tv)
    (v', s1, t1) <- local (applyTypes (`M.union` e)) $ tyInfer value

    Env env _ _ <- ask

    -- Typechecking variable type
    (s3, t2) <- case M.lookup name env of
          Just t' -> do
            t'' <- tyInstantiate t'
            let s = tyUnify t1 t''
                r = tyApply s t1
            unless (r == t'') $ error $ "Type " ++ show r ++ " does not match type " ++ show t''
            return (s `tyCompose` s1, r)
          Nothing -> return (s1, t1)

    let env'  = M.delete name env
        t'    = generalize (tyApply s3 env) t2
        env'' = M.insert name t' env'

    (b', s2, t2) <- local (applyTypes . const $ tyApply s1 env'') $ tyInfer body
    return (LetInE (name, t1) (tyApply s3 v') b', s3 `tyCompose` s2, t2)

  -- Data type inference
  tyInfer (A.Node (A.Literal "data") [dat, constructors, body]) = do
    (constr', _) <- parseData (parseTypeHeader dat) constructors
    local (applyCons (`M.union` constr')) $ tyInfer body

  tyInfer (A.List elems) = do
    (elems', s, t) <- foldlM (\(es, s, t) e -> do
      (e', s', t') <- tyInfer e
      return (e' : es, s `tyCompose` s', t' : t)) ([], M.empty, []) elems
    return (ListE elems' (head t), s, ListT (head t))

  -- Type inference for applications
  tyInfer (A.Node n [x]) = do
    tv <- tyFresh
    (n', s1, t1) <- tyInfer n
    (x', s2, t2) <- local (applyTypes (tyApply s1)) $ tyInfer x
    let s3   = tyUnify (tyApply s2 t1) (t2 :-> tv)
        x'' = tyApply s3 tv
    return (AppE n' x' x'', s3 `tyCompose` s2 `tyCompose` s1, x'')

  -- Value related inference
  tyInfer s@(A.String _) = return (LitE s String, M.empty, String)
  tyInfer i@(A.Integer _) = return (LitE i Int, M.empty, Int)
  tyInfer f@(A.Float _) = return (LitE f Float, M.empty, Float)
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
            let s = tyUnify t1 t''
                r = tyApply s t''
            return (s `tyCompose` s1, r)
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
