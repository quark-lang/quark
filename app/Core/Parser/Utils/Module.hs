{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import Core.Parser.AST (AST(..))
  import System.Directory (doesFileExist)
  import System.FilePath ((</>), dropFileName)
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.Combinator (runParser, many)
  import Data.Functor ((<&>))
  import Data.List
  import Data.Foldable
  
  parse :: String -> IO (Maybe AST)
  parse file = do
    exists <- doesFileExist file
    if exists
      then do
        content <- Parser.format <$> readFile file
        let (res, _) = runParser (many Parser.parse) content
        case res of
          Left err -> print err >> return Nothing
          Right ast -> do
            x <- visitAST (dropFileName file) (head ast)
            print x
            Just <$> visitAST (dropFileName file) (if length ast > 1 then Node "begin" ast else head ast)
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  -- take a source path and may return AST module
  resolveImport :: (String, String) -> IO (Maybe AST)
  resolveImport (base, path) = parse $ base </> path

  findIndexR :: (a->Bool) -> [a] -> Maybe Int
  findIndexR pred v = (length v-)
    <$> foldl (\a x -> if pred x
                        then Just 1
                        else succ<$>a) Nothing v

  slice :: [a] -> Int -> Int -> [a]
  slice xs i j = take (j - i) $ drop i xs

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p (Node n z) = do
    xs <- mapM (\case
      Node "import" [String path] -> do
        y <- resolveImport (p, path)
        print y 
        case y of
          Nothing -> return $ Node "import" [String path]
          Just ast -> return ast
      x -> return x) z
    print xs
    return $ Node n xs

  -- Value visiting
  visitAST _ i@(Integer _) = return i
  visitAST _ s@(String _) = return s
  visitAST _ f@(Float _) = return f
  visitAST _ s@(Literal _) = return s
