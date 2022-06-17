{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory       (doesFileExist)
  import System.FilePath        ((</>), dropFileName)
  import Core.Parser.Combinator (runParser, many)
  import Data.Functor           ((<&>))
  import Data.Foldable          (foldlM)
  import Data.Char              (ord)
  import System.Directory
  import System.Process
  import System.FilePath ((</>), (-<.>))
  import Core.Color

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
            let ast' = if length ast > 1 then Node (Literal "begin") [List ast] else head ast
              in Just <$> visitAST (dropFileName file) ast'
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node (Literal "import") [String path]) =
    return $ Node (Literal "import") [String $ p </> path]

  visitAST p z@(Node (Literal "begin") [List xs]) = do
    xs <- mapM (visitAST p) xs
    return $ Node (Literal "begin") [List xs]

  visitAST _ z = return z


  convertString :: String -> AST
  convertString s = Node (Literal "list") $ map Char s

  buildList :: [AST] -> AST
  buildList [] = Node (Literal "Nil") []
  buildList (x:xs) = Node (Literal "Cons") [x, buildList xs]
