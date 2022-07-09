module Core.Compiler.Compiler where
  import Prelude hiding (and)
  import Control.Monad.State
    (MonadIO, MonadState(get), StateT(runStateT))
  import qualified Data.Map as M
  import Control.Arrow (Arrow(second))
  import Data.Traversable (for)
  import Core.Inference.Type.AST (TypedAST(..), Literal(S))
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

  compile :: MonadCompiler m => TypedAST -> m Expression
  -- JS AST Generation
  compile (AppE (VarE "Call" _) [n] _) = Call <$> compile n <*> pure []
  compile (AppE (VarE "Call" _) [n, x] _) = Call <$> compile n <*> ((:[]) <$> compile x)
  compile (AppE (VarE "Property" _) [obj, prop] _) = Property <$> compile obj <*> compile prop
  compile (AppE (VarE "Var" _) [LitE (S x) _] _) = return $ Var x
  compile (AppE (VarE "Throw" _) [x] _) = Throw <$> compile x
  compile (AppE (VarE "Block" _) xs _) = Block <$> mapM compile xs
  compile (AppE (VarE "require" _) [LitE (S path) _] _) = return $ Require path
  compile (AppE (VarE "extern" _) [LitE (S content) _] _) = return $ Raw content

  -- Binary calls
  compile (AppE (VarE "*" _) [x, y] _) = BinaryCall <$> compile x <*> pure "*" <*> compile y
  compile (AppE (VarE "=" _) [x, y] _) = BinaryCall <$> compile x <*> pure "===" <*> compile y
  compile (AppE (VarE "!" _) [x] _)    = Call (Var "!") . (:[]) <$> compile x
  compile (AppE (VarE "-" _) [x, y] _) = BinaryCall <$> compile x <*> pure "-" <*> compile y
  compile (AppE (VarE "+" _) [x, y] _) = BinaryCall <$> compile x <*> pure "+" <*> compile y
  compile (AppE (VarE "/" _) [x, y] _) = BinaryCall <$> compile x <*> pure "/" <*> compile y

  compile (AppE (VarE n _) args _) = get >>= \e -> case M.lookup (varify n) e of
    -- Checking if it's a constructor
    Just obj -> Call (Property (Var obj) (Var $ varify n)) <$> mapM compile args
    Nothing -> Call (Var $ varify n) <$> mapM compile args
  compile (AppE n xs _) = Call <$> compile n <*> mapM compile xs
  compile (AbsE args body) = Lambda (map (varify . fst) args) <$> compile body
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

  runCompiler :: (Monad m, MonadIO m) => TypedAST -> Constructors -> m (Expression, Constructors)
  runCompiler a = runStateT (compile a)