{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory
  import System.FilePath
  import Data.Functor           ((<&>))
  import Data.Foldable
  import Data.Char              (ord)
  import System.Process
  import Core.Color
  import Data.List
  import System.Environment (lookupEnv)
  import Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle (ParseErrorBundle), ParseError (TrivialError), ErrorItem (EndOfInput, Tokens))
  import Data.Maybe (catMaybes)
  import qualified Data.Set as S
  import qualified Data.List.NonEmpty as N
  import Data.Void (Void)
  import Text.Megaparsec (Stream(Token))

  {-
    Module: Parser utils
    Description: Set of functions to extra-parse AST 
    Author: thomasvergne
  -}

  startsWith :: String -> String -> Bool
  startsWith p s = p `isPrefixOf` s

  endsWith :: String -> String -> Bool
  endsWith p s = p `isSuffixOf` s

  parse :: String -> IO (Maybe AST)
  parse file = do
    exists <- doesFileExist file
    if exists
      then do
        content <- readFile file
        let res = Parser.parseLisp content
        case res of
          Left err -> printErrorBundle err >> return Nothing
          Right ast -> do
            let ast' = catMaybes ast
            let ast'' = if length ast > 1 then Node (Literal "begin") [List ast'] else head ast'
              in Just <$> visitAST (dropFileName file) ast''
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node (Literal "import") [String path]) = do
    let path' = path ++ if endsWith ".qrk" path then "" else ".qrk"
    path'' <- if startsWith "std:" path'
      then lookupEnv "QUARK" >>= \case
        Just x -> do
          let path'' = x </> "std" </> drop 4 path'
          exists <- doesFileExist path''
          if exists
            then return path''
            else error $ "Directory " ++ path'' ++ " does not exist"
        Nothing -> error "QUARK environment variable is not set"
      else return $ p </> path'
    if startsWith "js:" path 
      then return $ Node (Literal "require") [String $ drop 3 path]
      else return $ Node (Literal "import") [String path'']

  visitAST p z@(Node (Literal "begin") [List xs]) = do
    xs <- mapM (visitAST p) xs
    return $ Node (Literal "begin") [List xs]

  visitAST _ z = return z

  showError :: ErrorItem (Token String) -> String
  showError EndOfInput = "Unexpected end of input"
  showError (Tokens x) = concatMap show x
  showError _ = "Unknown error"

  printErrorBundle :: ParseErrorBundle String a -> IO ()
  printErrorBundle x = do
    let (ParseErrorBundle errors _) = x
    traverse_ (\case
      TrivialError _ (Just msg) xs -> do
        let xs' = S.toList xs
        putStrLn $ red "[error] " ++ showError msg
        putStrLn $ "  in missing " ++ bBlack (concatMap showError xs')
      _ -> print "error") errors

  convertString :: String -> AST
  convertString s = Node (Literal "list") $ map Char s
