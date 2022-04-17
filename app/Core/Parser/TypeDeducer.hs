{-# LANGUAGE LambdaCase, BlockArguments, TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Core.Parser.TypeDeducer where
  import qualified Core.Parser.AST as A
  import Core.Parser.Macros
  import Control.Monad.RWS
  import Data.Maybe
  import qualified Data.Map as M
  import Data.List
  import Data.Functor
  import Debug.Trace (traceShow)
  import Data.Foldable

  -- typechecking transform a basic AST into a typed one
  data Type
    = Arrow Type Type
    | Var Int
    | String
    | Int
    | List Type
    | Void
    deriving Eq

  instance Show Type where
    show (Arrow ts t) = "(" ++ show ts ++ " -> " ++ show t ++ ")"
    show (Var n) = "f" ++ show n
    show String = "str"
    show Int = "int"
    show (List t) = "[" ++ show t ++ "]"
    show Void = "nil"

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

  -- Scheme represents a polymorphic type
  data Scheme = Scheme [Int] Type
    deriving (Show, Eq)

  class Types a where
    -- free variables consists of all variables that are not bound in the type
    free :: a -> [Int]
    -- substitute a type for a variable
    apply :: Substitution -> a -> a

  -- Some types class instances
  instance Types Type where
    free (Var n) = [n]
    free (Arrow t1 t2) = free t1 ++ free t2
    free (List t) = free t
    free _ = []

    apply s (Var n) = fromMaybe (Var n) (M.lookup n s)
    apply s (Arrow t1 t2) = Arrow (apply s t1) (apply s t2)
    apply s (List t) = List (apply s t)
    apply _ t = t
  instance Types Scheme where
    free (Scheme vars t) = filter (`notElem` vars) (free t)
    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)
  instance Types a => Types [a] where
    apply s = map (apply s)
    free = concatMap free

  -- Substitution is a mapping from typevariable to type
  type Substitution = M.Map Int Type

  nullSubstitution :: Substitution
  nullSubstitution = M.empty

  compose :: Substitution -> Substitution -> Substitution
  compose s1 s2 = M.map (apply s1) s2 `M.union` s1

  -- some state related useful types
  type Variable = (String, Scheme)
  type Environment = M.Map String Scheme
  type Deducer m = (MonadRWS Environment [(Type, Type)] Int m, MonadIO m)

  getType :: TypedAST -> Type
  getType (AppE _ _ t) = t
  getType (AbsE _ t) = getType t
  getType (VarE _ t) = t
  getType (LetE (_, t) _ _) = t
  getType (ListE _ t) = t
  getType (LitE _ t) = t

  remove :: Environment -> String -> Environment
  remove env name = M.delete name env

  instance Types Environment where
    free env = free (M.elems env)
    apply = M.map . apply

  -- abstracting a type over all free types
  generalize :: Environment -> Type -> Scheme
  generalize env t = Scheme vars t
    where vars = free t \\ free env

  -- generating a new typevar
  fresh :: Deducer m => m Type
  fresh = do
    n <- get
    modify (+1)
    return $ Var n

  -- replacing bound types with free ones
  instantiate :: Deducer m => Scheme -> m Type
  instantiate (Scheme vars t) = do
    n <- mapM (const fresh) vars
    let s = M.fromList $ zip vars n
    return $ apply s t

  unify :: Deducer m => Type -> Type -> m Substitution
  unify (Arrow a1 r1) (Arrow a2 r2) = do
    s1 <- unify a1 a2
    s2 <- unify (apply s1 r1) (apply s1 r2)
    return $ s2 `compose` s1
  unify (Var n) t = bindVariable n t
  unify t (Var n) = bindVariable n t
  unify (List t1) (List t2) = unify t1 t2
  unify Int Int = return nullSubstitution
  unify String String = return nullSubstitution
  unify t1 t2 = error $ "Cannot unify " ++ show t1 ++ " with " ++ show t2

  bindVariable :: Deducer m => Int -> Type -> m Substitution
  bindVariable n t
    | t == Var n = return nullSubstitution
    | n `elem` free t = error $ "Variable already bound: " ++ show n
    | otherwise = return $ M.singleton n t

  replaceType :: TypedAST -> (Type, Type) -> TypedAST
  replaceType (VarE n t) (t1, t2)
    | t == t1 = VarE n t2
    | otherwise = VarE n t
  replaceType (AppE f arg t) z = AppE (replaceType f z) (replaceType arg z) (if t == fst z then snd z else t)
  replaceType (AbsE (n, t) body) (t1, t2)
    = AbsE (n, if t == t1 then t2 else t) (replaceType body (t1, t2))
  replaceType (LetE (n, t) body t') (t1, t2)
    = LetE (n, if t == t1 then t2 else t) (replaceType body (t1, t2)) t'
  replaceType (ListE args t) (t1, t2)
    = ListE (map (`replaceType` (t1, t2)) args) (if t == t1 then t2 else t)
  replaceType (LitE ast t) (t1, t2)
    = LitE ast (if t == t1 then t2 else t)

  -- infering literal types
  literal :: Deducer m => A.AST -> m (Substitution, Type, TypedAST)
  literal (A.Integer i)
    = return (nullSubstitution, Int, LitE (A.Integer i) Int)
  literal (A.String s)
    = return (nullSubstitution, String, LitE (A.String s) String)
  literal _ = error "not a literal"

  infer :: Deducer m => A.AST -> m (Substitution, Type, TypedAST)
  infer (A.Literal n) = ask >>= \env ->
    case M.lookup n env of
      Nothing -> error "undefined variable"
      Just s -> do
        t <- instantiate s
        return (nullSubstitution, t, VarE n t)
  infer (A.Node (A.Literal "fn") [A.Literal arg, body]) = do
    tv  <- fresh
    env <- ask
    let env'  = remove env arg
        env'' = env' `M.union` M.singleton arg (Scheme [] tv)
    (s1, t1, b') <- local (const env'') (infer body)
    let t = apply s1 tv
    return (s1, Arrow t t1, AbsE (arg, t) (replaceType b' (tv, t)))
  infer (A.Node (A.Literal "let") [A.Literal name, value, expr]) = do
    tv <- fresh
    env <- ask >>= \env -> return $ env `M.union` M.fromList [(name, Scheme [] tv)]
    (s1, t1, v') <- local (const env) (infer value)
    let env'  = remove env name
        t'    = generalize (apply s1 env) t1
        env'' = M.insert name t' env'
    (s2, t2, e') <- local (const (apply s1 env'')) $ infer expr
    return (s1 `compose` s2, t2, LetE (name, t1) (replaceType v' (tv, apply s1 tv)) e')
  infer e@(A.Node n [x]) = do
    tv  <- fresh
    env <- ask
    (s1, t1, n') <- local (const env) $ infer n
    (s2, t2, x') <- local (const (apply s1 env)) $ infer x
    s3 <- unify (apply s2 t1) (Arrow t2 tv)
    let t = apply s3 tv
    return (s3 `compose` s2 `compose` s1, t, AppE n' x' t)
  infer (A.List args) = do
    a' <- mapM infer args
    liftIO $ print a'
    let t = map (\(_, x, _) -> x) a'
    unless (all (== head t) t) $ error "type mismatch"
    let v' = ListE (map (\(_, _, x) -> x) a') (List $ head t)
    return (nullSubstitution, List $ head t, v')
  infer x = literal x

  builtins =
    [
      ("+", Scheme [] (Arrow Int (Arrow Int Int))),
      --("begin", Scheme [0] (Arrow (List (Var 0)) (Var 0))),
      ("nil", Scheme [] Void)
    ]

  generate :: (Monad m, MonadIO m) => A.AST -> m TypedAST
  generate a = runRWST (infer a) (M.fromList builtins) 0 >>= \(x, a', b) -> do
    let (s, t, a) = x
    liftIO $ print a
    return a