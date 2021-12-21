module Core.Parser.Macros where
  import Core.Parser.AST (AST(..))
  import Control.Monad.State
  import Data.Foldable (find)

  {-
    Module: Macro processing
    Description: Process macro elimination and macro expansion
    Author: thomasvergne
  -}

  data Macro = Macro {
    name :: String,
    args :: [String],
    body :: AST
  } deriving (Show, Eq)

  type Macros = [Macro]
  type MacroST m a = StateT Macros m a

  registerMacro :: Monad m => Macro -> MacroST m ()
  registerMacro m = modify (m:)

  lookupMacro :: Monad m => String -> MacroST m (Maybe Macro)
  lookupMacro n = gets (find (\m -> name m == n))

  runMacroCompiler :: (Monad m, MonadIO m) => AST -> m AST
  runMacroCompiler a = removeMacros <$> evalStateT (compileMacro . fixUnrecursive $ fixLet a) []

  dropMacro :: Monad m => String -> MacroST m ()
  dropMacro n = modify (filter (\m -> name m /= n))

  common :: Eq a => [a] -> [a] -> [a]
  common xs ys = foldr (\x acc -> if x `elem` ys then x:acc else acc) [] xs

  unliteral :: AST -> String
  unliteral (Literal s) = s
  unliteral _ = error "Not a literal"

  compileMacro :: (Monad m, MonadIO m) => AST -> MacroST m AST
  -- processing unprocessed unrecursive lambda calls
  compileMacro z@(Node (Node (Literal "fn") [Node (Literal "list") args, body]) xs) = do
    xs' <- mapM compileMacro xs
    let args' = zip (map unliteral args) xs'
    return $ replaceMacroArgs body args'

  -- registering macro as a function
  compileMacro z@(Node (Literal "defm") [ Literal name, Node (Literal "list") args, body ]) = do
    registerMacro $ Macro name (map unliteral args) body
    return z

  -- registering simple macro variable
  compileMacro z@(Node (Literal "defm") [ Literal name, value ]) = do
    v' <- compileMacro value
    registerMacro $ Macro name [] v'
    return z
  
  -- processing begin by controling the macro flow scope
  compileMacro z@(Node (Literal "begin") xs) = do
    curr <- get
    xs'  <- mapM compileMacro xs
    new  <- get
    put $ common new curr
    return $ Node (Literal "begin") xs'

  -- processing function by registering arguments as macros
  -- for avoiding conflicts between other macros and arguments
  compileMacro (Node (Literal "fn") [Node (Literal "list") args, body]) = do
    mapM_ (\z@(Literal n) -> registerMacro (Macro n [] z)) args 
    xs <- compileMacro body
    mapM_ (dropMacro . unliteral) args
    return $ Node (Literal "fn") [Node (Literal "list") args, xs]

  compileMacro z@(Node (Literal n) xs) = do
    r <- lookupMacro n
    xs' <- mapM compileMacro xs
    case r of
      -- compiling macro call without explicit arguments
      Just (Macro _ [] body) -> do
        n' <- compileMacro body
        return $ Node n' xs'

      -- normally compiling macro
      Just (Macro _ args body) -> do
        let args' = zip args xs'
        compileMacro $ replaceMacroArgs body args'

      -- returning unmodified node if no macro found
      Nothing -> return $ Node (Literal n) xs'

  -- default case for Node processing
  compileMacro (Node n xs) = do
    xs' <- mapM compileMacro xs
    n'  <- compileMacro n
    compileMacro $ Node n' xs'

  -- modifying literal with eventual found macro
  compileMacro (Literal x) = do
    r <- lookupMacro x
    case r of
      Just (Macro _ _ body) -> return body
      Nothing -> return $ Literal x

  compileMacro x = return x

  type MacroVariable = (String, AST)

  -- function helper to replace macro arguments with call values
  replaceMacroArgs :: AST -> [MacroVariable] -> AST
  replaceMacroArgs (Node n xs) a = Node (replaceMacroArgs n a) (map (`replaceMacroArgs` a) xs)
  replaceMacroArgs (Literal n) a =
    let found = find (\(n', _) -> n == n') a
      in case found of
        Just (_, v) -> v
        Nothing -> Literal n
  replaceMacroArgs x _ = x
  
  findInAST :: AST -> String -> Bool
  findInAST (Node n xs) y = findInAST n y || any (`findInAST`y) xs 
  findInAST (Literal n) y = n == y
  findInAST _ _ = False

  -- replace unrecursive functions with macros
  fixUnrecursive :: AST -> AST
  fixUnrecursive z@(Node (Literal "const") [Literal name, Node (Literal "fn") [args, body]])
    = if findInAST body name
      then z
      else Node (Literal "defm") [Literal name, args, body]
  fixUnrecursive (Node (Literal "defn") [Literal name, args, body ])
    = if findInAST body name
      then Node (Literal "defn") [Literal name, args, body]
      else Node (Literal "defm") [Literal name, args, body]

  fixUnrecursive (Node n xs) = Node (fixUnrecursive n) (map fixUnrecursive xs)
  fixUnrecursive x = x

  isMacro :: AST -> Bool
  isMacro (Node (Literal "defm") _) = True
  isMacro _ = False

  removeMacros :: AST -> AST
  removeMacros (Node n xs) = Node n (filter (not . isMacro) (map removeMacros xs))
  removeMacros x = x
  
  isPure :: AST -> Bool
  isPure (Node (Literal "print") _) = False
  isPure (Node (Literal "input") _) = False
  isPure (Node n xs) = isPure n || all isPure xs
  isPure _ = True

  -- check if variable can be constant
  fixLet :: AST -> AST
  fixLet (Node (Literal "let") [Literal name, value])
    = if isPure value
        then Node (Literal "defm") [Literal name, value]
        else Node (Literal "let") [Literal name, value]

  fixLet (Node n xs) = Node (fixLet n) (map fixLet xs)
  fixLet x = x