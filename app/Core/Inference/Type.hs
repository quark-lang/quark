{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Core.Inference.Type where
  import qualified Data.Map as M
  import Data.Maybe (fromMaybe)
  import Data.List ((\\))
  import Control.Monad.RWS
    ( MonadIO (liftIO),
      modify,
      MonadState(get),
      RWST(runRWST),
      MonadRWS,
      MonadReader(local, ask), zipWithM )
  import qualified Core.Parser.AST as A
  import Core.Inference.Kind
  import Core.Parser.Macros
  import Data.Foldable
  import Control.Monad (forM, unless)
  import Debug.Trace
  
  type Argument = (String, Type)
  data TypedAST
    = AppE TypedAST TypedAST Type
    | AbsE Argument TypedAST
    | VarE String Type
    | LetInE Argument TypedAST TypedAST
    | ListE [TypedAST] Type
    | TLetE Argument TypedAST
    | LitE A.AST Type
    deriving Eq

  instance Show TypedAST where
    show (AppE f arg t) = "(" ++ show f ++ ") [" ++ show arg ++ "] : " ++ show t
    show (AbsE (n, t) body) = "(" ++ n ++ " : " ++ show t ++ ") -> " ++ show body
    show (VarE name t) = name ++ " : " ++ show t
    show (LetInE (name, t) body t') = "let " ++ name ++ " = " ++ show body ++ "\n  in " ++ show t'
    show (ListE args t) = "[" ++ show args ++ "]" ++ " : " ++ show t
    show (TLetE (name, t) body) = "let " ++ name ++ " = " ++ show body
    show (LitE ast t) = show ast ++ " : " ++ show t

  data Type
    = TVar Int | TId String
    | Type :-> Type
    | Int | String | Float
    | TApp Type Type
    deriving (Eq, Ord)

  instance Show Type where
    show (TVar i) = "t" ++ show i
    show (t1 :-> t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show Int = "Int"
    show Float = "Float"
    show String = "String"
    show (TApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
    show (TId s) = s

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord, Show)

  tyCompose :: SubTy -> SubTy -> SubTy
  tyCompose s1 s2 = M.map (tyApply s1) s2 `M.union` s1

  type SubTy   = M.Map Int Type
  type TypeEnv = M.Map String Scheme
  type ConsEnv = TypeEnv

  class Types a where
    tyFree  :: a -> [Int]
    tyApply :: SubTy -> a -> a
    tyUnify :: a -> a -> SubTy

  -- Unification variable helper
  variable :: Int -> Type -> SubTy
  variable n t
    | t == TVar n = M.empty
    | n `elem` tyFree t = error $ "Occurs check failed: " ++ show t
    | otherwise = M.singleton n t

  instance Types Type where
    tyFree (TVar i) = [i]
    tyFree (t1 :-> t2) = tyFree t1 ++ tyFree t2
    tyFree Int = []
    tyFree String = []
    tyFree (TApp t1 t2) = tyFree t1 ++ tyFree t2
    tyFree _ = []

    tyApply s (TVar i) = case M.lookup i s of
      Just t -> tyApply s t
      Nothing -> TVar i
    tyApply s (t1 :-> t2) = tyApply s t1 :-> tyApply s t2
    tyApply s (TApp t1 t2) = TApp (tyApply s t1) (tyApply s t2)
    tyApply _ s = s

    tyUnify (TVar i) t = variable i t
    tyUnify t (TVar i) = variable i t
    tyUnify (t1 :-> t2) (t3 :-> t4) =
      let s1 = tyUnify t1 t3
          s2 = tyUnify (tyApply s1 t2) (tyApply s1 t4)
        in s1 `tyCompose` s2
    tyUnify (TId s) (TId s') = if s == s'
      then M.empty
      else error $ "Type " ++ s ++ " mismatches with type " ++ s'
    tyUnify Int Int = M.empty
    tyUnify String String = M.empty
    tyUnify (TApp t1 t2) (TApp t3 t4) = tyUnify t1 t3 `M.union` tyUnify t2 t4
    tyUnify s1 s2 = error $ "Type " ++ show s1 ++ " mismatches with type " ++ show s2
  instance Types Scheme where
    tyFree (Forall v t) = tyFree t \\ v
    tyApply s (Forall v t) = Forall v (tyApply (foldr M.delete s v) t)
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types a => Types [a] where
    tyFree = concatMap tyFree
    tyApply = map . tyApply
    tyUnify _ _ = error "Cannot unify type scheme"
  instance Types TypeEnv where
    tyFree = tyFree . M.elems
    tyApply = M.map . tyApply
    tyUnify = error "Cannot unify type environment"
  instance Types TypedAST where
    tyFree (AppE f arg t) = tyFree f ++ tyFree arg
    tyFree (AbsE (n, t) body) = (tyFree t \\ tyFree t) ++ tyFree body
    tyFree (VarE name t) = tyFree t
    tyFree (LetInE (name, t) body t') = (tyFree t \\ tyFree t) ++ tyFree body ++ tyFree t'
    tyFree (TLetE (name, t) body) = (tyFree t \\ tyFree t) ++ tyFree body
    tyFree (ListE args t) = tyFree t ++ concatMap tyFree args
    tyFree (LitE ast t) = tyFree t

    tyApply s (AppE f arg t) = AppE (tyApply s f) (tyApply s arg) (tyApply s t)
    tyApply s (AbsE (n, t) body) = traceShow (s, t, tyApply s t) $ AbsE (n, tyApply s t) (tyApply s body)
    tyApply s (VarE name t) = VarE name (tyApply s t)
    tyApply s (LetInE (name, t) body t') = LetInE (name, tyApply s t) (tyApply s body) (tyApply s t')
    tyApply s (ListE args t) = ListE (map (tyApply s) args) (tyApply s t)
    tyApply s (TLetE (name, t) body) = TLetE (name, tyApply s t) (tyApply s body)
    tyApply s (LitE ast t) = LitE ast (tyApply s t)

    tyUnify _ _ = error "Cannot unify AST"

  -- Generalization is the process of managing flexible type variables
  -- as rigid type variables.
  generalize :: TypeEnv -> Type -> Scheme
  generalize env t = Forall vars t
    where vars = tyFree t \\ tyFree env

  -- Creating a new fresh type variable
  tyFresh :: MonadType m => m Type
  tyFresh = get >>= \n -> modify (+1) >> return (TVar n)

  -- Create a type instance based on a scheme
  tyInstantiate :: MonadType m => Scheme -> m Type
  tyInstantiate (Forall vars t) = do
    vars' <- mapM (const tyFresh) vars
    let s = M.fromList $ zip vars vars'
      in return $ tyApply s t

  -- Some environment functions and types
  applyTypes :: (TypeEnv -> TypeEnv) -> Env -> Env
  applyTypes f (Env ty cons k) = Env (f ty) cons k

  applyCons :: (ConsEnv -> ConsEnv) -> Env -> Env
  applyCons f (Env ty cons k) = Env ty (f cons) k

  applyKinds :: (KindEnv -> KindEnv) -> Env -> Env
  applyKinds f (Env ty cons k) = Env ty cons (f k)

  data Env = Env TypeEnv ConsEnv KindEnv
  type MonadType m = (MonadRWS Env () Int m, MonadIO m)

  buildDataType :: String -> [Type] -> Type
  buildDataType name [] = TId name
  buildDataType name (a:rgs) = foldl TApp (TApp (TId name) a) rgs

  parseConstructor :: Type -> M.Map String Type -> [A.AST] -> Type
  parseConstructor d m xs
    = let xs' = map (parseType m) xs
        in foldr (:->) d xs'
  
  buildFun :: [Type] -> Type
  buildFun [] = error "Cannot build function type"
  buildFun [t] = t
  buildFun (t:ts) = t :-> buildFun ts

  parseType :: M.Map String Type -> A.AST -> Type
  parseType e (A.Node (A.Literal "->") xs) = buildFun (map (parseType e) xs)
  parseType e (A.Node n xs) = foldl TApp (parseType e n) (map (parseType e) xs)
  parseType e (A.List [x]) = TApp (TId "List") (parseType e x)
  parseType e (A.Literal "str") = String
  parseType e (A.Literal "int") = Int
  parseType e (A.Literal n) = case M.lookup n e of
    Nothing -> TId n
    Just i  -> i
  parseType _ _ = error "Invalid constructor"

  -- Main type inference function
  tyInfer :: MonadType m => A.AST -> m (TypedAST, SubTy, Type)
  -- Type inference for variables
  tyInfer (A.Literal n) = ask >>= \(Env t c _) -> case M.lookup n (t `M.union` c) of
    Just t -> do
      t' <- tyInstantiate t
      return (VarE n t', M.empty, t')
    Nothing -> error $ "Variable " ++ show n ++ " is not defined."

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
    constr' <- parseData (parseTypeHeader dat) constructors
    local (applyCons (`M.union` constr')) $ tyInfer body

  -- Type inference for applications
  tyInfer (A.Node n [x]) = do
    tv <- tyFresh
    (n', s1, t1) <- tyInfer n
    (x', s2, t2) <- local (applyTypes (tyApply s1)) $ tyInfer x
    let s3 = tyUnify (tyApply s2 t1) (t2 :-> tv)
    let appTy = tyApply s3 tv
    return (AppE n' x' appTy, s3 `tyCompose` s2 `tyCompose` s1, appTy)

  -- Value related inference
  tyInfer s@(A.String _) = return (LitE s String, M.empty, String)
  tyInfer i@(A.Integer _) = return (LitE i Int, M.empty, Int)
  tyInfer f@(A.Float _) = return (LitE f Float, M.empty, Float)
  tyInfer a = error $ "AST node not supported: " ++ show a

  functions = [
      ("+", Forall [] (Int :-> (Int :-> Int)))
    ]

  parseData :: MonadType m => (String, [String]) -> A.AST -> m TypeEnv
  parseData (name, tyArgs) (A.List constructors) = do
    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
    let tyVars   = map (argsMap M.!) tyArgs
        dataType = buildDataType name tyVars
        schemeV  = map (\(TVar n) -> n) tyVars
        schemeCt = Forall schemeV

    constr' <- forM constructors $ \case
      A.Node (A.Literal name) args -> do
        let consTy = parseConstructor dataType argsMap args
          in return (name, schemeCt consTy)
      A.Literal name -> return (name, schemeCt dataType)
      _ -> error "Invalid constructor"
    
    return $ M.fromList constr'
  parseData _ _ = error "Invalid data type"
  
  topLevel :: MonadType m => A.AST -> m (Maybe TypedAST, Env)
  -- Top-level data with constructors
  topLevel (A.Node (A.Literal "data") [dat, constructors]) = do
    constr' <- parseData (parseTypeHeader dat) constructors
    e <- ask
    return (Nothing, applyCons (`M.union` constr') e)

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
        
    return (Just $ TLetE (name, t2) (tyApply s3 v'), Env env'' c k)
  
  -- Top-level declare used to define function type
  topLevel z@(A.Node (A.Literal "declare") [dat, def]) = do
    let (name, tyArgs) = parseTypeHeader dat
    
    argsMap <- M.fromList <$> mapM ((`fmap` tyFresh) . (,)) tyArgs
  
    let ty = parseType argsMap def
    e@(Env t _ _) <- ask
    let ty' = generalize t ty
    return (Nothing, applyTypes (`M.union` M.singleton name ty') e)
  topLevel x = error $ "Invalid top level expression, received: " ++ show x

  parseTypeHeader :: A.AST -> (String, [String])
  parseTypeHeader (A.Node (A.Literal name) args) = (name, map unliteral args)
  parseTypeHeader (A.Literal name) = (name, [])
  parseTypeHeader _ = error "Invalid type header"

  emptyEnv :: Env
  emptyEnv = Env M.empty M.empty M.empty

  mergeEnv :: Env -> Env -> Env
  mergeEnv (Env t c f) (Env t' c' f') = Env (t `M.union` t') (c `M.union` c') (f `M.union` f')

  runInfer :: MonadIO m => A.AST -> m ()
  runInfer a = do
    (a', env) <- case a of 
      A.Node (A.Literal "begin") [A.List xs] -> 
        foldlM (\(a, e) x -> do
          ((a', e'), _, _) <- runRWST (topLevel x) e 0
          return (case a' of
            Nothing -> a
            Just ast -> a ++ [ast], mergeEnv e e')) ([], emptyEnv) xs
      x -> do
        ((a', e), _, _) <- runRWST (topLevel x) emptyEnv 0
        return (case a' of
          Nothing -> []
          Just ast -> [ast], e)
    liftIO $ print a'
