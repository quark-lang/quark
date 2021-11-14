{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory       (doesFileExist)
  import System.FilePath        ((</>), dropFileName)
  import Core.Parser.Combinator (runParser, many)
  import Data.Functor           ((<&>))
  import Data.Foldable          (foldlM)
  import Data.Char

  {-
    Module: Parser utils
    Description: Set of functions to extra-parse AST 
    Author: thomasvergne
  -}

  parse :: String -> IO (Maybe AST)
  parse file = do
    exists <- doesFileExist file
    if exists
      then do
        content <- Parser.format <$> readFile file
        let (res, _) = runParser (many Parser.parse) content
        case res of
          Left err -> print err >> return Nothing
          Right ast ->
            let ast' = if length ast > 1 then Node "begin" ast else head ast
              in Just <$> visitAST (dropFileName file) ast'
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  -- take a source path and may return AST module
  resolveImport :: (String, String) -> IO (Maybe AST)
  resolveImport (base, path) = parse $ base </> path

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node "import" [String path]) =
    resolveImport (p, path) >>= \case
      -- if importing does not work, return the original node
      Nothing -> return z
      -- return a node which gonna be spread
      Just ast -> return $ Node "spread" [ast]

  -- Converting list to their Cons/Nil representation
  visitAST p (Node "list" xs) = visitAST p $ convertList xs

  visitAST p a@(Node n z) = do
    -- building new children by folding
    xy <- foldlM (\a x -> do
      x <- visitAST p x
      case x of
        -- spreading function just put 
        -- children content in the current built
        Node "spread" [Node "begin" xs] -> return $ a ++ xs
        _ -> return $ a ++ [x]) [] z
    return $ Node n xy

  -- Value visiting
  visitAST _ i@(Integer _) = return i
  -- Converting string to list of chars
  visitAST p   (String s)  = visitAST p $ convertString s
  visitAST _ f@(Float _)   = return f
  visitAST _ s@(Literal _) = return s
  -- Converting char to integer
  visitAST _   (Char c)    = return . Node "chr" $ [Integer . toInteger . ord $ c]

  convertList :: [AST] -> AST
  convertList [] = Literal "Nil"
  convertList (x:xs) = Node "Cons" [x, convertList xs]

  convertString :: String -> AST
  convertString s = Node "list" $ map Char s

  removeDuplicates :: Eq a => [a] -> [a]
  removeDuplicates = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

  removeOne :: Eq a => a -> [a] -> [a]
  removeOne x xs = filterOnce x xs 0
    where filterOnce :: Eq a => a -> [a] -> Int -> [a]
          filterOnce _ [] _ = []
          filterOnce x (y:ys) i = if (y == x) && (i == 0) then filterOnce x ys 1 else y : filterOnce x ys i


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

  garbageCollection x = x
