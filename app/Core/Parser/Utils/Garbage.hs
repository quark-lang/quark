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

  filterOnce :: (a -> Bool) -> [a] -> [a]
  filterOnce f xs = f' f xs 0
    where f' :: (a -> Bool) -> [a] -> Int -> [a]
          f' _ [] _ = []
          f' f (x:xs) i = if f x && i == 0
            then x : f' f xs (i + 1)
            else f' f xs i

  removeOne :: Eq a => a -> [a] -> [a]
  removeOne x = filterOnce (== x)

  type Variables = [String]
  type GarbageState m a = StateT Variables m a

  addVariable :: Monad m => String -> GarbageState m ()
  addVariable = modify . (:)

  dropVariable :: Monad m => String -> GarbageState m ()
  dropVariable = modify . removeOne

  lookupVariable :: Monad m => String -> GarbageState m Bool
  lookupVariable x = get <&> (x `elem`)

  removeUnusedVariables :: (Monad m, MonadIO m) => AST -> GarbageState m AST
  removeUnusedVariables z@(Node (Literal "begin") xs) = do
    -- getting current environment
    curr <- get
    xs'  <- mapM removeUnusedVariables xs
    new  <- get
    -- get begin environment
    let beginEnv = curr \\ new
    let begin = filter (\case
          Node (Literal "let") (Literal name:_) -> notElem name beginEnv
          _ -> True) xs'
    
    return $ Node (Literal "begin") begin

  removeUnusedVariables (Node (Literal "let") [Literal name, value]) = do
    -- adding variable to environment
    addVariable name
    -- removing unused variables
    val <- removeUnusedVariables value
    return $ Node (Literal "let") [Literal name, val]

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
        Node (Literal "begin") [x] ->
          return $ acc ++ [x]
        _ -> return $ acc ++ [x']) [] xs
    return $ Node n' xs'

  scopeElimination x = return x

  runGarbageCollector :: (Monad m, MonadFail m, MonadIO m) => AST -> m AST
  runGarbageCollector a = do
    a' <- evalStateT (removeUnusedVariables a) []
    Node (Literal "spread") xs <- evalStateT (scopeElimination a') []
    return a'