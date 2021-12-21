module Core.Parser.Macros where
  import Core.Parser.AST (AST(..))
  import Control.Monad.State
    (MonadIO, gets, modify, evalStateT, MonadState(put, get), StateT)
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
  runMacroCompiler a = removeMacros <$> evalStateT (compileMacro $ fixUnrecursive a) []

  dropMacro :: Monad m => String -> MacroST m ()
  dropMacro n = modify (filter (\m -> name m /= n))

  common :: Eq a => [a] -> [a] -> [a]
  common xs ys = foldr (\x acc -> if x `elem` ys then x:acc else acc) [] xs

  unliteral :: AST -> String
  unliteral (Literal s) = s
  unliteral _ = error "Not a literal"

  compileMacro :: (Monad m, MonadIO m) => AST -> MacroST m AST
  compileMacro z@(Node (Literal "defm") [ Literal name, Node (Literal "list") args, body ]) = do
    registerMacro $ Macro name (map unliteral args) body
    return z

  compileMacro z@(Node (Literal "defm") [ Literal name, value ]) = do
    v' <- compileMacro value
    registerMacro $ Macro name [] v'
    return z
  
  compileMacro z@(Node (Literal "begin") xs) = do
    curr <- get
    xs'  <- mapM compileMacro xs
    new  <- get
    put $ common new curr
    return $ Node (Literal "begin") xs'

  compileMacro (Node (Literal "fn") [Node (Literal "list") args, body]) = do
    mapM_ (\z@(Literal n) -> registerMacro (Macro n [] z)) args 
    xs <- compileMacro body
    mapM_ (\(Literal n) -> dropMacro n) args
    return $ Node (Literal "fn") [Node (Literal "list") args, xs]

  compileMacro z@(Node (Literal n) xs) = do
    r <- lookupMacro n
    xs' <- mapM compileMacro xs
    case r of
      Just (Macro _ args body) -> do
        let args' = zip args xs'
        compileMacro $ replaceMacroArgs body args'
      Nothing -> return $ Node (Literal n) xs'

  compileMacro (Node n xs) = do
    xs' <- mapM compileMacro xs
    n'  <- compileMacro n
    return $ Node n' xs'

  compileMacro (Literal x) = do
    r <- lookupMacro x
    case r of
      Just (Macro _ _ body) -> return body
      Nothing -> return $ Literal x

  compileMacro x = return x

  type MacroVariable = (String, AST)

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