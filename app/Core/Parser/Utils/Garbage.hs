module Core.Parser.Utils.Garbage where
  import Core.Parser.AST (AST(..))
  
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

  toList :: AST -> [AST]
  toList (Node "Cons" [x, Literal "Nil"])  = [x]
  toList (Node "Cons" [x, xs]) = x : toList xs
  toList _ = error "is not a list"

  garbageCollection :: AST -> AST
  garbageCollection p@(Node "begin" xs) = do
    let xs' = foldl (\acc x -> case garbageCollection x of
                Node "begin" xs -> acc ++ xs
                x -> acc ++ [x]) [] xs

    let vars =
          removeDuplicates $ foldl (\a x -> case x of
            Node "drop" [name] -> removeOne name (reverse a)
            Node "let" (name:_:_) -> name : a
            _ -> a) [] xs'

    Node "begin" $ xs' ++ map (\x -> Node "drop" [x]) vars
  garbageCollection (Node "fn" [args, p]) = do
    let xs'   = garbageCollection p
    -- dropping function arguments
    let args' = map (\x -> Node "drop" [x]) (toList args)
    case xs' of 
      Node "begin" xs -> Node "fn" (args : xs ++ args')
      _ -> Node "fn" ([args, xs'] ++ args')
      
  garbageCollection (Node n z) = Node n $ map garbageCollection z
  garbageCollection x = x
