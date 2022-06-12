{-# LANGUAGE TupleSections, LambdaCase #-}
module Core.Compiler.CLang where
  import Data.List
  import Data.Char
  import Control.Monad.State
  import Core.Parser.Utils.ClosureConversion hiding (lambdaCount, environment, Name)
  import Core.Inference.Type.AST
  import Data.Maybe (fromMaybe)
  import qualified Data.Map as M
  import qualified Core.Parser.AST as A
  import Control.Arrow (Arrow(second))
  import Data.Traversable (for)

  {-
    Module: CLang compilation
    Description: Output C from Quark AST
    Author: thomasvergne
  -}

  type MonadCompiler m = (MonadState CompilerState m, Monad m, MonadIO m)
  data CompilerState = CompilerState {
    types :: [String],
    functions :: M.Map String Type,
    counter :: Int,
    environment :: [String]
  } deriving Show

  -- State functions
  addType :: MonadCompiler m => String -> m ()
  addType t = modify $ \s -> s { types = t : types s }

  addVar :: MonadCompiler m => String -> m ()
  addVar v = modify $ \s -> s { environment = v : environment s }

  setEnv :: MonadCompiler m => [String] -> m ()
  setEnv e = modify $ \s -> s { environment = e }

  addFun :: MonadCompiler m => (String, Type) -> m ()
  addFun (f, t) = modify $ \s -> s { functions = M.insert f t (functions s) }

  incLambda :: MonadCompiler m => m Int
  incLambda = gets counter <* modify (\s -> s { counter = counter s + 1 })

  -- Type translation
  numberToChar :: Int -> Char
  numberToChar n = chr $ ord 'A' + n

  createTypeTable :: Type -> M.Map Int Char
  createTypeTable (t1 :-> t2) = M.union (createTypeTable t1) (createTypeTable t2)
  createTypeTable (TVar t) = M.singleton t (numberToChar t)
  createTypeTable _ = M.empty

  fromType :: MonadCompiler m => Type -> M.Map Int Char -> m String
  fromType Int _ = return "int"
  fromType String _ = return "char*"
  fromType Float _ = return "float"
  fromType (t1 :-> t2) _ = do
    t1' <- fromType t1 $ createTypeTable t1
    t2' <- fromType t2 $ createTypeTable t2
    return $ "std::function<" ++ t1' ++ "(" ++ t2' ++ ")>"
  fromType (TVar t) e = case M.lookup t e of
    Just c -> return [c]
    Nothing -> return "void*"
  fromType _ _ = return "auto"

  {- MISC PART -}

  libraries = ["iostream"]
  operators = ["+", "-", "/", "*", "==", "!=", ">", "<", ">=", "<="]

  loadLibrary :: String -> String
  loadLibrary l = "#include <" ++ l ++ ">"

  getReturnType :: Type -> Type
  getReturnType (t1 :-> t2) = t2
  getReturnType t = t

  matchType :: Type -> Type -> M.Map Type Type
  matchType (t1 :-> t2) (t1' :-> t2') = M.union (matchType t1 t1') (matchType t2 t2')
  matchType (TVar t) (TVar t') = M.singleton (TVar t) (TVar t')
  matchType (TVar t) t1 = M.singleton (TVar t) t1
  matchType t1 (TVar t) = M.singleton (TVar t) t1
  matchType _ _ = M.empty

  {- COMPILATION PHASE -}

  compileLambda :: MonadCompiler m => TypedAST -> m (String, String)
  compileLambda (AbsE (name, t) body) = do
    env <- gets environment
    addVar name

    (body', t') <- compile body False
    argTy       <- fromType t (createTypeTable t)
    let captures = intercalate "," env

    setEnv env
    return ("[" ++ captures ++ "](" ++ argTy ++ " " ++ name ++ ") { return " ++ body' ++ "; }", t')

  compileCase :: MonadCompiler m => (TypedAST, TypedAST) -> String -> m String
  compileCase (pattern, body) v = case pattern of
    (VarE name t) -> do
      addVar name
      (body', _) <- compile body False
      t' <- fromType t (createTypeTable t)
      return $ t' ++ " " ++ name ++ " = " ++ v ++ ";\n" ++ "return " ++ body' ++ ";"
    (LitE l _) -> do
      (body', _) <- compile body False
      let lit = compileLiteral l
      return $ "if(" ++ v ++ " == " ++ lit ++ ") { return " ++ body' ++ "; }"

  compileBegin :: MonadCompiler m => [TypedAST] -> Bool -> m (String, String)
  compileBegin xs b = do
    xs' <- mapM ((fst <$>) . (`compile` b)) xs
    return ("{" ++ concat (map (++";") xs') ++ "}", "void")

  compile :: MonadCompiler m => TypedAST -> Bool -> m (String, String)
  -- Main entry
  compile (LetE ("main", _) (AbsE (arg, _) body)) b = do
    (body', t') <- compile body b
    return ("int main(int argc, char** " ++ arg ++ ") { " ++ body' ++ ";}", t')

  -- Top-level functions
  compile (LetE (name, t) a@(AbsE (arg, ty) body)) b = do
    env <- gets environment
    addVar arg

    addFun (name, t)

    let t' = getReturnType t
    let tt = createTypeTable t' `M.union` createTypeTable ty
    t  <- fromType t' tt
    ty <- fromType ty tt
    (body', t') <- compile body b

    setEnv env
    let template
          = if M.null tt
              then ""
              else let typenames = (M.elems (M.map (\x -> "typename " ++ [x]) tt))
                in "template<" ++ intercalate "," typenames ++ ">\n"
        header = template ++ t ++ " " ++ name ++ "(" ++ ty ++ " " ++ arg ++ ")"
    case body of
      PatternE _ _ -> return (header ++ "{" ++ body' ++ "}", t')
      AppE (VarE "extern" _) _ _ -> return (header ++ "{" ++ body' ++ "}", t')
      _ -> return (header ++ "{ return " ++ body' ++ " ;}", t')

  compile (PatternE pattern cases) _ = do
    (pattern, t) <- compile pattern False
    xs <- mapM (`compileCase` pattern) cases
    return (intercalate "\n" xs, t)

  -- Variable definition
  compile (LetE (name, t) value) b = do
    t' <- fromType t (createTypeTable t)
    (v, t2) <- compile value b

    return ("auto " ++ name ++ " = " ++ v, t2)

  compile (AppE (VarE "extern" _) (LitE (A.String s) _) _) _ =
    return (s, "")

  compile (LetInE (name, t) value body) _ = do
    t' <- fromType t (createTypeTable t)
    (v, t2) <- compile value False
    (body', t3) <- compile body False
    return ("([](" ++ t' ++ " " ++ name ++ "){" ++ body' ++ ";})(" ++ v ++ ")", t3)

  -- Lambda definition
  compile a@(AbsE _ _) _ = compileLambda a

  compile (IfE cond then' else') b = do
    (cond', t) <- compile cond b
    (then', t2) <- compile then' b
    (else', t3) <- compile else' b
    return (cond' ++ " ? " ++ then' ++ " : " ++ else' ++ ";", t3)

  -- Binary function call
  compile (AppE (AppE (VarE op t) x _) y _) b = case find (== op) operators of
    Just _ -> do
      (x', t1) <- compile x b
      (y', t2) <- compile y b
      return ("(" ++ x' ++ " " ++ op ++ " " ++ y' ++ ")", t1)
    Nothing -> gets functions
      >>= \f -> case M.lookup op f of
        Just t1 -> do
          let types = M.elems (matchType t t1)
          types <- mapM (`fromType` M.empty) types
          (x', t1) <- compile x b
          (y', t2) <- compile y b
          return (op ++ (if b then "<" ++ intercalate "," types ++ ">" else "") ++ "(" ++ x' ++ ")(" ++ y' ++ ")", t1)
        Nothing -> error $ "Unknown function: " ++ op

  compile (AppE (VarE n t) x _) b = do
    gets functions
      >>= \f -> case M.lookup n f of
        Just t1 -> do
          let types = M.elems (matchType t t1)
          types <- mapM (`fromType` M.empty) types
          (x', t1) <- compile x b
          return (n ++ (if b then "<" ++ intercalate "," types ++ ">" else "") ++ "(" ++ x' ++ ")", t1)
        Nothing -> do
          (x', t1) <- compile x b
          return (n ++ "(" ++ x' ++ ")", t1)



  -- Function call
  compile (AppE n x _) b = do
    (n, t) <- compile n b
    (x, t') <- compile x (not b)
    return (n ++ "(" ++ x ++ ")", t)

  compile (VarE n t) _ = (n,) <$> fromType t (createTypeTable t)
  compile (LitE l t) _ = (compileLiteral l,) <$> fromType t (createTypeTable t)
  compile x _ = error $ "Pattern not supported: " ++ show x

  compileLiteral :: A.AST -> String
  compileLiteral (A.Char c) = show c
  compileLiteral (A.Integer i) = show i
  compileLiteral (A.Float f) = show f
  compileLiteral (A.String s) = show s
  compileLiteral _ = ""

  formatProgram :: [String] -> String
  formatProgram = unlines . ((map loadLibrary libraries) ++)

  runCompiler :: (Monad m, MonadIO m) => TypedAST -> M.Map String Type -> m (String, M.Map String Type)
  runCompiler a f = do
    ((r, _), CompilerState _ f _ _) <- runStateT (compile a True) (CompilerState [] f 0 [])
    return (r, f)
