{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Compiler where
  import qualified Core.Parser.AST as A
  import Control.Monad.State
  import GHC.Float (float2Double)
  import Core.Parser.Macros (unliteral)
  import Data.Functor ((<&>))

  data CompilerState = CompilerState {
    bound :: [(String, String)],
    stateID :: Int
  } deriving Show

  type Compiler m = (MonadState CompilerState m, MonadIO m)

  -- Representing a Javascript AST
  data AST
    -- Expressions
    = FunctionCall AST [AST]
    | Identifier String
    | String String
    | Number Double
    | Lambda [String] AST
    | BinaryExpression String AST AST
    | Block [AST]

    -- Statements
    | Assignment String AST
    | Condition AST AST AST
    | Delete AST
    deriving Show

  addVariable :: Compiler m => String -> m (String, String)
  addVariable name = do
    bound <- gets bound
    id <- gets stateID
    modify $ \s -> s { stateID = id + 1, bound = (name, "_v" ++ show id) : bound }
    return (name, "_v" ++ show id)

  popVariable :: Compiler m => m (String, String)
  popVariable =
    gets bound >>= \case
      [] -> error "No variables to pop"
      (x:xs) -> do
        modify $ \s -> s { bound = xs }
        return x

  compile :: Compiler m => A.AST -> m AST
  compile (A.Node (A.Literal "begin") xs) = do
    before <- gets stateID
    xs' <- mapM compile xs
    after  <- gets stateID
    replicateM_ (after - before) popVariable
    return $ Block xs'

  compile (A.Node (A.Literal "let") [A.Literal name, value]) = do
    (_, id) <- addVariable name
    return <$> Assignment id =<< compile value

  compile (A.Node (A.Literal "fn") [A.Node (A.Literal "list") args, body]) = do
    before <- gets stateID
    args' <- mapM (\(A.Literal name) -> addVariable name <&> snd) args
    xs <- compile body
    after <- gets stateID
    replicateM_ (after - before) popVariable
    return $ Lambda args' xs

  compile (A.Node (A.Literal "if") [cond, then_, else_]) =
    Condition <$> compile cond <*> compile then_ <*> compile else_

  compile (A.Node (A.Literal "drop") [name]) = Delete <$> compile name

  compile (A.Node n xs) =
    compile n >>= \case
      x@(Identifier i) -> if i `elem` binaryExprs
        then BinaryExpression i <$> compile (head xs) <*> compile (last xs)
        else FunctionCall x <$> mapM compile xs
      x -> FunctionCall x <$> mapM compile xs

  compile (A.Literal "nil") = return $ Identifier "null"

  compile (A.Literal s) = do
    bound <- gets bound
    return $ case lookup s bound of
      Just id -> Identifier id
      Nothing -> Identifier s

  compile (A.Float n) = return . Number . float2Double $ n
  compile (A.String s) = return $ String s
  compile (A.Integer n) = return . Number . fromIntegral $ n

  compile n = liftIO $ print n >> return (Block [])
  
  binaryExprs :: [String]
  binaryExprs = ["+", "-", "*", "/", ">", "=", "or", "and"]

  runCompiler :: MonadIO m => A.AST -> m AST
  runCompiler a = evalStateT (compile a) (CompilerState [("print", "console.log")] 0)