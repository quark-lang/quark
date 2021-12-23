{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Garbage where
  import Core.Parser.AST (AST(..))
  import Control.Monad.State
  import Core.Parser.Macros (common)
  import Data.List ((\\))
  import Data.Functor ((<&>))

  {-
    Module: Garbage collection
    Description: Scope elimination by garbage collection which is a process of adding drop call to the AST
    Author: thomasvergne
  -}

  removeDuplicates :: Eq a => [a] -> [a]
  removeDuplicates = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

  removeOne :: Eq a => a -> [a] -> [a]
  removeOne x xs = filterOnce x xs 0
    where filterOnce :: Eq a => a -> [a] -> Int -> [a]
          filterOnce _ [] _ = []
          filterOnce x (y:ys) i = if (y == x) && (i == 0) then filterOnce x ys 1 else y : filterOnce x ys i

  type Variables = [String]
  type GarbageState m a = StateT Variables m a

  addVariable :: Monad m => String -> GarbageState m ()
  addVariable = modify . (:)

  dropVariable :: Monad m => String -> GarbageState m ()
  dropVariable = modify . removeOne

  lookupVariable :: Monad m => String -> GarbageState m Bool
  lookupVariable x = get <&> (x `elem`)

  removeUnusedVariables :: Monad m => AST -> GarbageState m AST
  removeUnusedVariables (Node (Literal "begin") xs) = do
    curr <- get
    xs' <- mapM (\case
      x@(Node (Literal "let") (Literal name:_)) -> addVariable name >> return x
      x -> removeUnusedVariables x) xs
    new <- get
    let scope_variables = new \\ curr
    let xs'' = filter (\case
                Node (Literal "let") (Literal name:_) -> notElem name scope_variables
                _ -> True) xs'
    return $ Node (Literal "begin") xs''

  removeUnusedVariables (Node n xs) = do
    n' <- removeUnusedVariables n
    xs' <- mapM removeUnusedVariables xs
    return $ Node n' xs'

  removeUnusedVariables (Literal n) =
    lookupVariable n >>= \x ->
      when x (dropVariable n) >> return (Literal n)

  removeUnusedVariables x = return x

  scopeElimination :: Monad m => AST -> GarbageState m AST
  scopeElimination (Node (Literal "begin") xs) = do
    curr <- get
    xs' <- foldM (\acc x -> do
      x' <- scopeElimination x
      case x' of
        (Node (Literal "spread") xs) -> return $ acc ++ xs
        _ -> return $ acc ++ [x']) [] xs
    new <- get
    return $ Node (Literal "spread") (xs' ++ map (\x -> Node (Literal "drop") [Literal x]) (new \\ curr))

  scopeElimination z@(Node (Literal "let") (Literal name:value:_)) = do
    addVariable name
    xs' <- scopeElimination value >>= \case
      (Node (Literal "spread") xs) -> return xs
      x -> return [x]
    return $ Node (Literal "let") (Literal name:xs')
  
  scopeElimination (Node n xs) = do
    n' <- scopeElimination n
    xs' <- foldM (\acc x -> do
      x' <- scopeElimination x
      case x' of
        Node (Literal "spread") [x] -> 
          return $ acc ++ [x]
        Node (Literal "spread") xs -> 
          return $ acc ++ [Node (Literal "begin") xs]
        _ -> return $ acc ++ [x']) [] xs
    return $ Node n' xs'

  scopeElimination x = return x

  runGarbageCollector :: (Monad m, MonadFail m, MonadIO m) => AST -> m AST
  runGarbageCollector a = do
    a' <- evalStateT (removeUnusedVariables a) []
    Node (Literal "spread") xs <- evalStateT (scopeElimination a') []
    return $ Node (Literal "begin") xs