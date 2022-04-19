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
  import Control.Monad (forM)
  
  type Argument = (String, Type)
  data TypedAST
    = AppE TypedAST TypedAST Type
    | AbsE Argument TypedAST
    | VarE String Type
    | LetE Argument TypedAST TypedAST
    | ListE [TypedAST] Type
    | LitE A.AST Type
    deriving Eq

  instance Show TypedAST where
    show (AppE f arg t) = "(" ++ show f ++ ") [" ++ show arg ++ "] : " ++ show t
    show (AbsE (n, t) body) = "(" ++ n ++ " : " ++ show t ++ ") -> " ++ show body
    show (VarE name t) = name ++ " : " ++ show t
    show (LetE (name, t) body t') = "let " ++ name ++ " = " ++ show body ++ "\n  in " ++ show t'
    show (ListE args t) = "[" ++ show args ++ "]" ++ " : " ++ show t
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

    tyApply s (TVar i) = fromMaybe (TVar i) (M.lookup i s)
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
    tyFree (LetE (name, t) body t') = (tyFree t \\ tyFree t) ++ tyFree body ++ tyFree t'
    tyFree (ListE args t) = tyFree t ++ concatMap tyFree args
    tyFree (LitE ast t) = tyFree t

    tyApply s (AppE f arg t) = AppE (tyApply s f) (tyApply s arg) (tyApply s t)
    tyApply s (AbsE (n, t) body) = AbsE (n, tyApply s t) (tyApply s body)
    tyApply s (VarE name t) = VarE name (tyApply s t)
    tyApply s (LetE (name, t) body t') = LetE (name, tyApply s t) (tyApply s body) (tyApply s t')
    tyApply s (ListE args t) = ListE (map (tyApply s) args) (tyApply s t)
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
    = let xs' = map helper xs
        in foldr (:->) d xs'
    where helper :: A.AST -> Type
          helper (A.Node (A.Literal "->") [t1, t2]) = helper t1 :-> helper t2
          helper (A.Node n x)  = TApp (helper n) (parseConstructor d m x)
          helper (A.Literal "str") = String
          helper (A.Literal "int") = Int
          helper (A.Literal n) = case M.lookup n m of
            Nothing -> TId n
            Just i  -> i
          helper _ = error "Invalid constructor"

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
    let env'  = M.delete name env
        t'    = generalize (tyApply s1 env) t1
        env'' = M.insert name t' env'

    (b', s2, t2) <- local (applyTypes . const $ tyApply s1 env'') $ tyInfer body
    return (LetE (name, t1) (tyApply s1 v') b', s1 `tyCompose` s2, t2)

  -- Data type inference
  tyInfer (A.Node (A.Literal "data") [dat, A.List constructors, body]) = do
    let (name, tyArgs) = case dat of
          A.Node (A.Literal name) args -> (name, map unliteral args)
          A.Literal name -> (name, [])
          _ -> error "Invalid data type"

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
    
    let consEnv = M.fromList constr'
      in local (applyCons (`M.union` consEnv)) $ tyInfer body


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

  runInfer :: MonadIO m => A.AST -> m Type
  runInfer a = do
    let env = Env (M.fromList functions) M.empty M.empty
    ((a, s, t), _, _) <- runRWST (tyInfer a) env 0
    liftIO $ print a
    return $ tyApply s t
