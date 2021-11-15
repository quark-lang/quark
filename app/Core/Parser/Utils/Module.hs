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
            let ast' = if length ast > 1 then Node (Literal "begin") ast else head ast
              in Just <$> visitAST (dropFileName file) ast'
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  -- take a source path and may return AST module
  resolveImport :: (String, String) -> IO (Maybe AST)
  resolveImport (base, path) = parse $ base </> path

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node (Literal "import") [String path]) =
    resolveImport (p, path) >>= \case
      -- if importing does not work, return the original node
      Nothing -> return z
      -- return a node which gonna be spread
      Just ast -> return $ Node (Literal "spread") [ast]

  -- Converting list to their Cons/Nil representation
  visitAST p (Node (Literal "list") xs) = visitAST p $ convertList xs

  visitAST p (Node (Literal "let") (name:value:rest:_))
    = visitAST p $ 
      Node (Literal "let") [
        name,
        Node (Node (Literal "fn") [
          Node (Literal "Cons") [name, Literal "Nil"], 
          rest]) [value]]

  visitAST p a@(Node n z) = do
    -- building new children by folding
    xy <- foldlM (\a x -> do
      x <- visitAST p x
      case x of
        -- spreading function just put 
        -- children content in the current built
        Node (Literal "spread") [Node (Literal "begin") xs] -> return $ a ++ xs
        _ -> return $ a ++ [x]) [] z
    r <- visitAST p n
    return $ Node (case r of
      Node (Literal "spread") [xs] -> xs
      _ -> r) xy

  -- Value visiting
  visitAST _ i@(Integer _) = return i
  -- Converting string to list of chars
  visitAST p   (String s)  = visitAST p $ convertString s
  visitAST _ f@(Float _)   = return f
  visitAST _ s@(Literal _) = return s
  -- Converting char to integer
  visitAST _   (Char c)    = return . Node (Literal "chr") $ [Integer . toInteger . ord $ c]

  convertList :: [AST] -> AST
  convertList [] = Literal "Nil"
  convertList (x:xs) = Node (Literal "Cons") [x, convertList xs]

  convertString :: String -> AST
  convertString s = Node (Literal "list") $ map Char s
  