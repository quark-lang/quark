{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory       (doesFileExist)
  import System.FilePath        ((</>), dropFileName)
  import Core.Parser.Combinator (runParser, many)
  import Data.Functor           ((<&>))
  import Data.Foldable          (foldlM)
  
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
  visitAST _ s@(String _) = return s
  visitAST _ f@(Float _) = return f
  visitAST _ s@(Literal _) = return s
