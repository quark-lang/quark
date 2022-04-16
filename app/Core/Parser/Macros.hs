{-# LANGUAGE LambdaCase #-}
module Core.Parser.Macros where
  import Core.Parser.AST (AST(..))
  import Control.Monad.State
  import Data.Foldable (find)
  import Data.List (find, isPrefixOf, (\\))

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
  type MacroST m = (MonadState Macros m, MonadIO m)

  registerMacro :: MacroST m => Macro -> m ()
  registerMacro m = modify (m:)

  lookupMacro :: MacroST m => String -> m (Maybe Macro)
  lookupMacro n = gets . find $ (==n) . name

  runMacroCompiler :: (Monad m, MonadIO m) => AST -> m AST
  runMacroCompiler a = removeMacroDefinition <$> evalStateT (compileMacro a) []

  filterOnce :: (a -> Bool) -> [a] -> [a]
  filterOnce _ [] = []
  filterOnce f (x:xs) = if f x then xs else x : filterOnce f xs

  dropMacro :: MacroST m => String -> m ()
  dropMacro n = put . filterOnce ((==n) . name) =<< get

  isMacroDefinition :: AST -> Bool
  isMacroDefinition (Node (Literal "defm") _) = True
  isMacroDefinition _ = False

  removeMacroDefinition :: AST -> AST
  removeMacroDefinition (Node n xs) = 
    let n'  = removeMacroDefinition n
        xs' = filter (not . isMacroDefinition) xs
      in Node n' $ map removeMacroDefinition xs'
  removeMacroDefinition z = z

  common :: Eq a => [a] -> [a] -> [a]
  common xs ys = filter (`elem` ys) xs

  unliteral :: AST -> String
  unliteral (Literal s) = s
  unliteral _ = error "Not a literal"

  impureFunctions = ["print", "read", "readInt", "readFloat", "readBool", "readChar", "readString"]

  isPure :: AST -> Bool
  isPure (Node (Literal n) xs) = notElem n impureFunctions && all isPure xs
  isPure (Node n xs) = isPure n && all isPure xs
  isPure _ = True

  isRecursive :: AST -> String -> Bool
  isRecursive (Node (Literal n) xs) n' = n == n' || any (`isRecursive` n') xs
  isRecursive (Node n xs) n' = isRecursive n n' || any (`isRecursive` n') xs
  isRecursive _ _ = False

  compileMacro :: MacroST m => AST -> m AST
  compileMacro z@(Node (Literal "defm") [Literal name, value]) = do
    value <- compileMacro value
    registerMacro $ Macro name [] value
    return z

  compileMacro z@(Node (Literal "defm") [Literal name, Node (Literal "list") args, body]) = do
    -- saving arguments as macros
    mapM_ (\x -> registerMacro $ Macro (unliteral x) [] x) args
    body' <- compileMacro body
    -- removing macros arguments
    mapM_ (dropMacro . unliteral) args
    registerMacro $ Macro name (map unliteral args) body'
    return z
  compileMacro (List xs) = List <$> mapM compileMacro xs
  compileMacro (Node (Literal "drop") [Literal name]) = 
    lookupMacro name >>= \case
      Nothing -> return $ Node (Literal "drop") [Literal name]
      Just m -> do
        dropMacro name
        return $ Literal "nil"

  compileMacro z@(Node (Literal n) xs) = do
    lookupMacro n >>= \case
      Just (Macro _ args body) -> do
        let args' = zip args xs
        compileMacro $ replaceArgumentsWithValue body args'
      Nothing -> Node (Literal n) <$> mapM compileMacro xs
  
  compileMacro (Node n xs) = Node <$> compileMacro n <*> mapM compileMacro xs
  compileMacro (Literal s) = do
    lookupMacro s >>= \case
      Just (Macro _ _ value) -> return value
      Nothing -> return $ Literal s
  compileMacro z = return z

  replaceArgumentsWithValue :: AST -> [(String, AST)] -> AST
  replaceArgumentsWithValue (Node n xs) args =
    let n'  = replaceArgumentsWithValue n args
        xs' = map (`replaceArgumentsWithValue` args) xs
      in Node n' xs'
  replaceArgumentsWithValue (List xs) args = List (map (`replaceArgumentsWithValue` args) xs)
  replaceArgumentsWithValue (Literal n) args =
    case lookup n args of
      Just x -> x
      Nothing -> Literal n
  replaceArgumentsWithValue z _ = z

