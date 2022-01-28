module Core.Compiler.CLang where
  import Data.List
  import Data.Char
  import Control.Monad.State
  import Core.Parser.TypeDeducer
  import Core.Parser.Utils.ClosureConversion
  {-
    Module: CLang compilation
    Description: Output C from Quark AST
    Author: thomasvergne
  -}

  type LambdaType = (String, String, String)

  data C = C {
    lambdaTypes :: [LambdaType],
    functions :: [String],
    temporary :: [String]
  } deriving Show

  addTemporary :: Compiler m => String -> m String
  addTemporary t = (modify $ \c -> c { temporary = temporary c ++ [t] }) >> return t

  clearTemporary :: Compiler m => m ()
  clearTemporary = modify $ \c -> c { temporary = [] }

  addLambdaType :: Compiler m => LambdaType -> m ()
  addLambdaType s = modify $ \c -> c { lambdaTypes = s : lambdaTypes c }

  addFunction :: Compiler m => String -> m ()
  addFunction s = modify $ \c -> c { functions = s : functions c }

  type Compiler m = (MonadState C m, MonadIO m, MonadFail m)

  compile :: Compiler m => TypedAST -> m String
  compile x = return "t"

  runCompiler :: (Monad m, MonadIO m, MonadFail m) => [Closure] -> m C
  runCompiler ((Closure _ _ _ x):xs) = execStateT (compile x) (C [] [] [])