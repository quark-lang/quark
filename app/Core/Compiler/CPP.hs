{-# LANGUAGE LambdaCase #-}
module Core.Compiler.CPP where
  import Control.Monad.State
  import qualified Data.Map as M
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty ()
  import Data.List (intercalate, union)
  import Data.Functor
  import Control.Arrow (Arrow(first))
  
  {- TYPE DECLARATION -}

  type Environment  = M.Map String Type
  type Constructors = M.Map String String
  type MonadCPP m   = (MonadState (Environment, Constructors) m, MonadIO m)

  add :: MonadCPP m => String -> Type -> m ()
  add name t = modify . first $ M.insert name t

  remove :: MonadCPP m => String -> m ()
  remove name = modify . first $ M.delete name

  search :: MonadCPP m => String -> m (Maybe Type)
  search name = gets fst <&> M.lookup name

  replace :: MonadCPP m => Environment -> m ()
  replace = modify . first . const

  isGeneric :: MonadCPP m => String -> m Bool
  isGeneric s = search s <&> maybe False isGenericType

  isGenericType :: Type -> Bool
  isGenericType = not . null . templateMap

  {- TYPE COMPILATION -}

  fromType :: Type -> String
  fromType (TVar i) = "A" ++ show i
  fromType (args :-> ret) = "std::function<" ++ fromType ret ++ "(" ++ intercalate "," (map fromType args) ++ ")>"
  fromType Int = "int"
  fromType Bool = "bool"
  fromType String = "char*"
  fromType Float = "float"
  fromType (TApp t1 t2) = fromType t1 ++ (if null t2 then "" else " <" ++ intercalate "," (map fromType t2) ++ ">")
  fromType _ = "void*"

  unions :: Eq a => [[a]] -> [a]
  unions = foldl union []

  templateMap :: Type -> [String]
  templateMap (t1 :-> t2) = unions (map templateMap t1) `union` templateMap t2
  templateMap (TVar t) = ["T" ++ show t]
  templateMap _ = []

  template :: Type -> String
  template t = if null m then "" else "template<" ++ intercalate "," m ++ ">"
    where m = map (++"typename ") $ templateMap t

  {- CPP COMPILATION -}

  compileCase :: MonadCPP m => TypedPattern -> m (String -> String -> String)
  compileCase (VarP name _) = return $ \x body -> 
    "auto" ++ name ++ " = " ++ x ++ ";" ++ body ++ ";"
  compileCase (LitP l _) = return $ \x body -> 
    "if(" ++ x ++ " == " ++ compileLiteral l ++ ") { return " ++ body ++ "; }"
  compileCase (WilP _) = return $ \x body -> 
    "return " ++ body ++ ";"
  compileCase _ = return $ \_ _ -> ""

  compilePattern :: MonadCPP m => TypedAST -> m String
  compilePattern (PatternE x pats) = do
    x' <- compile x
    pats' <- mapM (\(p, b) -> do
      b <- compile b
      p <- compileCase p
      return $ p x' b) pats
    return $ "([" ++ x' ++ "]() {" ++ intercalate "\n" pats' ++ "})()"
  compilePattern _ = error "compilePattern: not a pattern"

  compile :: MonadCPP m => TypedAST -> m String
  compile z@(PatternE _ _) = compilePattern z
  compile (AppE (VarE "print" _) args _) = do
    args' <- mapM compile args
    return $ "std::cout << " ++ intercalate " << " args' ++ " << std::endl"
  compile (AppE (VarE "*" _) args _) = do
    args' <- mapM compile args
    return $ "(" ++ intercalate " * " args' ++ ")"
  compile (AppE (VarE "-" _) args _) = do
    args' <- mapM compile args
    return $ "(" ++ intercalate " - " args' ++ ")"
  compile (AppE n xs _) = do
    n' <- compile n
    xs' <- mapM compile xs
    isGeneric n' <&> \case
      True -> n' ++ "<" ++ intercalate "," (map (fromType . getType) xs) ++ ">" ++ "(" ++ intercalate "," xs' ++ ")"
      False -> n' ++ "(" ++ intercalate "," xs' ++ ")"
  compile (AbsE args body) = do
    env <- gets fst 
    let names = intercalate ", " $ M.keys env
        args' = intercalate "," $ map (\(x, t) -> fromType t ++ " " ++ x) args
        temp  = template (getType body)
    body' <- compile body
    return $ "[" ++ names ++ "]" ++ temp ++ "(" ++ args' ++ "){ return " ++ body' ++ "; }"
  compile (LetE ("main", _) (AbsE _ body)) = do
    body' <- compile body
    return $ "int main(){" ++ body' ++ "; }"
  compile (LetE (n, t) body) = do
    body' <- compile body
    return $ fromType t ++ " " ++ n ++ " = " ++ body'
  compile (VarE n _) = return n
  compile (LitE l _) = return $ compileLiteral l
  compile _ = return ""

  compileLiteral :: Literal -> String
  compileLiteral (I i) = show i
  compileLiteral (F f) = show f
  compileLiteral (S s) = show s
  compileLiteral (C c) = show c

  runCompiler :: MonadIO m => TypedAST -> (Environment, Constructors) -> m (String, (Environment, Constructors))
  runCompiler t = runStateT (compile t)