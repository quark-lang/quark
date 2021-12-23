{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Parser.Utils.ClosureConversion where
  import Core.Parser.AST (AST(..))
  import Core.Parser.Utils.Garbage (removeDuplicates, removeOne)
  import Control.Monad.State
  import Core.Parser.Macros (unliteral, common)
  import Data.List
  
  {-
    Module: Closure Conversion
    Description: Defunctionnalizing the lambdas and closures by adding closure environment to the call arguments and putting them at top level.
    Author: thomasvergne
  -}

  {-
    Closure conversion should not put closure environment at beggining of lambda arguments because it would break partial application (currying). So we're building a tuple in which first member is the closure environment and the second represents the lambda itself.
  -}

  -- Closure type
  type Environment = [String]
  type Closure = (Environment, AST)

  -- Rebuilding a custom AST to simplify closure compilation and keeping free code structure
  data ClosuredNode
    = Closure String [String] Closure -- name, args, body
    | Application AST
    deriving Show
  type ClosuredAST = [ClosuredNode]

  -- Closure conversion states
  data ClosureConversion = ClosureConversion {
    environment :: Environment,
    ast :: ClosuredAST,
    lambdaCount :: Int,
    symbolTable :: [(String, String)]
  } deriving Show

  type ClosureST m = (MonadState ClosureConversion m, MonadIO m, MonadFail m)

  -- Some useful state functions
  getEnvironment :: ClosureST m => m Environment
  getEnvironment = gets environment

  addEnvironment :: ClosureST m => String -> m ()
  addEnvironment x = modify $ \s -> s { environment = x : environment s }

  removeEnvironment :: ClosureST m => m String
  removeEnvironment = do
    env <- getEnvironment
    case env of
      [] -> error "No environment variable to remove"
      (x:xs) -> do
        modify $ \s -> s { environment = xs }
        return x

  addClosure :: 
    ClosureST m 
      => Closure
      -> [String]
      -> m String
  addClosure (env, a) ar = do
    cst <- get
    let name = "lambda" ++ show (lambdaCount cst)
    modify $ \s -> s { 
      lambdaCount = lambdaCount cst + 1,
      ast = Closure name ar (env, a) : ast s
    }
    return name
  
  getNextName :: ClosureST m => m String
  getNextName = do
    cst <- get
    return $ "lambda" ++ show (lambdaCount cst + 1)

  nextName :: ClosureST m => m String
  nextName = do
    cst <- get
    modify $ \s -> s { lambdaCount = lambdaCount cst + 1 }
    return $ "lambda" ++ show (lambdaCount cst)

  addSymbol :: ClosureST m => (String, String) -> m ()
  addSymbol x = modify $ \s -> s { symbolTable = x : symbolTable s }

  filterOnce :: (a -> Bool) -> [a] -> [a]
  filterOnce f xs = f' f xs 0
    where f' :: (a -> Bool) -> [a] -> Int -> [a]
          f' _ [] _ = []
          f' f (x:xs) i = if f x && i == 0 
            then x : f' f xs (i + 1) 
            else f' f xs i
          

  removeSymbol :: ClosureST m => String -> m ()
  removeSymbol x = modify $ \s -> s { symbolTable = filterOnce (\(a, b) -> a /= x) (symbolTable s) }

  replaceClosureName :: AST -> (String, String) -> AST
  replaceClosureName (Literal n) (name, replace) =
    if n == name then Literal replace else Literal n
  replaceClosureName (Node n xs) i = 
    Node 
      (replaceClosureName n i)
      (map (`replaceClosureName` i) xs) 
  replaceClosureName x _ = x

  lookupClosure :: ClosureST m => String -> m Closure
  lookupClosure name = do
    cst <- gets ast
    case find (\case
      Closure n _ _ -> n == name
      _ -> False) cst of
      Nothing -> error $ "Closure " ++ name ++ " not found"
      Just (Closure _ _ n) -> return n
      Just _ -> error "Closure not found"

  lookupSymbol :: ClosureST m => String -> m String
  lookupSymbol name = do
    cst <- gets symbolTable
    case find (\case
      (a, b) -> a == name) cst of
      Nothing -> return name
      Just (a, b) -> return b

  convert :: ClosureST m => AST -> m AST
  -- Special case for defn 
  convert z@(Node (Literal "let") [Literal name, Node (Literal "fn") [Node (Literal "list") args, body]]) = do
    next <- nextName
    addEnvironment next
    
    -- Saving environment before computing function conversion
    env <- getEnvironment

    -- Adding arguments to environment
    let args' = map unliteral args
    mapM_ addEnvironment args'

    -- Adding function name and its real lambda converted name
    addSymbol (name, next)
    body' <- convert body

    -- Removing the lambda environment
    current <- getEnvironment
    let env' = common env current
    modify $ \s -> s { environment = env' }

    -- Building and inserting closure
    let closure = Closure next args' (env \\ current, body')
    modify $ \s -> s { ast = closure : ast s }
    return z
  
  -- Adding variable to the environment when defined
  convert (Node (Literal "let") [Literal name, value]) = do
    addEnvironment name
    convert value

  convert z@(Node (Literal "fn") [Node (Literal "list") args, body]) = do
    env <- getEnvironment
    let args' = map unliteral args
    mapM_ addEnvironment args'

    body' <- convert body

    -- Removing the lambda environment
    current <- getEnvironment
    let env' = common env current
    modify $ \s -> s { environment = env' }

    -- Building and inserting closure
    name <- addClosure (env', body') args'
    return $ Literal name
    
    
  convert (Node (Literal "drop") [x]) = do
    Literal name <- convert x
    removeSymbol name
    return $ Node (Literal "drop") [Literal name]

  convert (Node n xs) = Node <$> convert n <*> mapM convert xs
  convert (Literal name) = Literal <$> lookupSymbol name
  convert x = return x

  runClosureConverter :: (Monad m, MonadIO m, MonadFail m) => AST -> m ClosureConversion
  runClosureConverter x = execStateT (convert x) (ClosureConversion [] [] 0 [])