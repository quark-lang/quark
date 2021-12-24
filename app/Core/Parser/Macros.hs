{-# LANGUAGE LambdaCase #-}
module Core.Parser.Macros where
  import Core.Parser.AST (AST(..))
  import Control.Monad.State
  import Data.Foldable (find)
  import Data.List
  import Data.Maybe
  
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

  dropMacro :: (Monad m, MonadIO m) => String -> MacroST m ()
  dropMacro n = do
    macros <- get
    put $ f macros n False
    where
      f :: [Macro] -> String -> Bool -> [Macro]
      f [] _ _ = []
      f (m:ms) n i = if name m == n && not i
        then ms
        else m : f ms n i

  common :: Eq a => [a] -> [a] -> [a]
  common xs ys = foldr (\x acc -> if x `elem` ys then x:acc else acc) [] xs

  unliteral :: AST -> String
  unliteral (Literal s) = s
  unliteral _ = error "Not a literal"

  compileMacro :: (Monad m, MonadIO m) => AST -> MacroST m AST
  -- processing unprocessed unrecursive lambda calls
  compileMacro z@(Node (Node (Literal "fn") [Node (Literal "list") args, body]) xs) = do
    let args' = zip (map unliteral args) xs
    compileMacro $ replaceMacroArgs body args'

  compileMacro (Node (Literal "let") [Literal name, value]) = do
    lookupMacro name >>= \case
      Just m -> dropMacro name
      Nothing -> return ()
    value' <- compileMacro value
    return $ Node (Literal "let") [Literal name, value']
  
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
    case r of
      -- compiling macro call without explicit arguments
      Just (Macro _ [] body) -> do
        if findInAST body n 
          then compileMacro body
          else return body
      -- normally compiling macro
      Just (Macro _ args body) -> do
        let args' = zipArguments args xs
        compileMacro $ replaceMacroArgs body args'

      -- returning unmodified node if no macro found
      Nothing -> Node (Literal n) <$> mapM compileMacro xs

  -- default case for Node processing
  compileMacro (Node n xs) = do
    xs' <- mapM compileMacro xs
    n'  <- compileMacro n
    return $ Node n' xs'

  -- modifying literal with eventual found macro
  compileMacro (Literal x) = do
    --when (x == "x") $ do
    --  lookupMacro x >>= liftIO . print
    lookupMacro x >>= \case
      Just (Macro _ _ body) -> case body of
        z@(Literal n) -> if n == x 
          then return z 
          else compileMacro z
        _ -> compileMacro body
      Nothing -> return $ Literal x
  
  compileMacro x = return x

  type MacroVariable = (String, AST)

  -- function helper to replace macro arguments with call values
  replaceMacroArgs :: AST -> [MacroVariable] -> AST
  replaceMacroArgs (Node n xs) a = Node (replaceMacroArgs n a) (map (`replaceMacroArgs` a) xs)
  replaceMacroArgs (Literal n) a =
    let found = find (\(n', _) -> remove "..." n == n') a
      in case found of
        Just (_, v) -> 
          if "..." `isPrefixOf` n 
            then case v of
              Node (Literal "list") xs -> Node (Literal "spread") xs
              _ -> v  
            else v
        Nothing -> Literal n
  replaceMacroArgs x _ = x
  
  remove :: String -> String -> String
  remove w "" = ""
  remove w s@(c:cs) 
    | w `isPrefixOf` s = remove w (drop (length w) s)
    | otherwise = c : remove w cs

  -- zipping arguments with taking care of spread arguments
  zipArguments :: [String] -> [AST] -> [(String, AST)]
  zipArguments [x] z@(y:ys) =
    if "..." `isPrefixOf` x
      then [(remove "..." x, Node (Literal "list") z)]
      else [(x, y)]
  zipArguments (x:xs) (y:ys) = (x, y) : zipArguments xs ys
  zipArguments (x:_) [] = [(x, Literal "Nil")]
  zipArguments [] _ = []

  findInAST :: AST -> String -> Bool
  findInAST (Node (Literal n) xs) y = n == y || any (`findInAST`y) xs
  findInAST (Node n xs) y = findInAST n y || any (`findInAST`y) xs
  findInAST _ _ = False

  -- replace unrecursive functions with macros
  fixUnrecursive :: AST -> AST
  fixUnrecursive z@(Node (Literal "const") [Literal name, Node (Literal "fn") [args, body]])
    = if findInAST body name
      then z
      else Node (Literal "defm") [Literal name, args, body]
  fixUnrecursive (Node (Literal "defn") [Literal name, args, body ])
    = if findInAST body name
      then Node (Literal "let") [Literal name, Node (Literal "fn") [args, body]]
      else Node (Literal "defm") [Literal name, args, body]

  fixUnrecursive (Node n xs) = Node (fixUnrecursive n) (map fixUnrecursive xs)
  fixUnrecursive x = x

  isMacro :: AST -> Bool
  isMacro (Node (Literal "defm") _) = True
  isMacro _ = False

  removeMacros :: AST -> AST
  removeMacros (Node n xs) = Node (removeMacros n) (filter (not . isMacro) (map removeMacros xs))
  removeMacros x = x

  isPure :: AST -> Bool
  isPure (Node (Literal "print") _) = False
  isPure (Node (Literal "input") _) = False
  isPure (Node n xs) = isPure n || all isPure xs
  isPure _ = True

  -- check if variable can be constant
  fixLet :: AST -> AST
  fixLet (Node (Literal "let") [Literal name, value])
    = if isPure value && not (findInAST (fixLet value) name)
        then Node (Literal "defm") [Literal name, fixLet value]
        else Node (Literal "let") [Literal name, fixLet value]

  fixLet (Node n xs) = Node (fixLet n) (map fixLet xs)
  fixLet x = x