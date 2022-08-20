module Core.Compiler.Compiler where
  import Prelude hiding (and)
  import Control.Monad.State
    (MonadIO (liftIO), MonadState(get), StateT(runStateT))
  import qualified Data.Map as M
  import Control.Arrow (Arrow(second))
  import Data.Traversable (for)
  import Core.Inference.Type.AST (TypedAST(..), Literal(S, C), Type (String, Char, TId, TApp))
  import Core.Compiler.Definition.Generation (varify)
  import Core.Compiler.Definition.IR
    (Expression(..), MonadCompiler, Constructors)
  import Core.Compiler.Modules.Pattern (compileCase)
  import Core.Compiler.Modules.ADT (compileData)
  
  {-
    Module: CLang compilation
    Description: Output C from Quark AST
    Author: thomasvergne
  -}

  compilePattern :: MonadCompiler m => TypedAST -> m Expression
  compilePattern (PatternE x pats) = do
    x <- compile x
    Call <$> (Lambda [] . Block <$> mapM (\(p, b) -> do
      b <- compile b
      p <- compileCase p
      return $ p x b) pats) <*> pure []
  compilePattern _ = error "compilePattern: not a pattern"

  fromList :: TypedAST -> [TypedAST]
  fromList (VarE "Nil" _) = []
  fromList (AppE (VarE "Cons" _) [x, xs] _) = x : fromList xs
  fromList _ = error "fromList: not a list"

  fromString :: TypedAST -> String
  fromString (VarE "Nil" _) = []
  fromString (AppE (VarE "Cons" _) [LitE (C c) _, xs] _) = c : fromString xs
  fromString x = error $ "Not a string " ++ show x

  isString :: TypedAST -> Bool
  isString (VarE "Nil" (TApp (TId "Cons") [Char])) = True
  isString (AppE (VarE "Cons" _) [LitE (C c) _, xs] _) = True
  isString _ = False

  compile :: MonadCompiler m => TypedAST -> m Expression
  compile (AppE (AppE (AppE (VarE "binary" _) [x] _) [z] _) [y] _) 
    = BinaryCall <$> compile x <*> pure (fromString z) <*> compile y
  compile (AppE (VarE "call" _) [n] _) 
    = Call <$> compile n <*> pure []
  compile (AppE (AppE (VarE "call" _) [n] _) [x] _) 
    = Call <$> (if isString n then return (Var (fromString n)) else compile n) <*> ((:[]) <$> compile x)
  compile (AppE (AppE (VarE "property" _) [obj] _) [prop] _) = do
    obj <- if isString obj then pure (Var (fromString obj)) else compile obj
    prop <- if isString prop then pure (Var (fromString prop)) else compile prop
    return $ Property obj prop
  compile (AppE (AppE (VarE "index" _) [obj] _) [prop] _) = do
    obj <- if isString obj then pure (Var (fromString obj)) else compile obj
    prop <- if isString prop then pure (Var (fromString prop)) else compile prop
    return $ Index obj prop
  compile (AppE (VarE "throw" _) [x] _) 
    = Block . (:[]) . Throw <$> compile x
  compile (AppE (AppE (AppE (VarE "condition" _) [cond] _) [then'] _) [else'] _)
    = Ternary <$> compile cond <*> compile then' <*> compile else'

  compile (AppE (VarE n _) args _) = get >>= \e -> case M.lookup (varify n) e of
    -- Checking if it's a constructor
    Just obj -> Call (Property (Var obj) (Var $ varify n)) <$> mapM compile args
    Nothing -> Call (Var $ varify n) <$> mapM compile args
  compile (AppE n xs _) = Call <$> compile n <*> mapM compile xs
  compile (AbsE args body) = Lambda (map (varify . fst) args) <$> compile body
  compile (VarE "true" _) = return $ Var "true"
  compile (VarE "false" _) = return $ Var "false"
  compile (VarE t _) = get >>= \e -> case M.lookup (varify t) e of
    Just obj -> return $ Property (Var obj) (Var $ varify t)
    Nothing -> return . Var . varify $ t
  compile (LetInE (n, _) value expr) = do
    value <- compile value
    expr  <- compile expr
    return $ Call (Lambda [varify n] expr) [Call (Lambda [] $ Block [Let (varify n) value, Return (Var $ varify n)]) []]
  compile (ListE exprs _) = Array <$> mapM compile exprs
  compile (LetE (n, _) value) = Let (varify n) <$> compile value
  compile (LitE l _) = return $ Lit l
  compile z@(DataE _ _) = compileData z
  compile z@(PatternE _ _) = compilePattern z
  compile (InstE t _) = error $ "Should not appear in compilation: " ++ t

  runCompiler :: (Monad m, MonadIO m) => TypedAST -> Constructors -> m (Expression, Constructors)
  runCompiler a = runStateT (compile a)