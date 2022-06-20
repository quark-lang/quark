{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory
  import System.FilePath
  import Data.Functor           ((<&>))
  import Data.Foldable          (foldlM)
  import Data.Char              (ord)
  import System.Process
  import Core.Color
  import Data.List  
  import System.Environment (lookupEnv)
  import Text.Megaparsec.Error (errorBundlePretty)
  import Data.Maybe (catMaybes)
  
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
          Left err -> putStrLn (errorBundlePretty err) >> return Nothing
          Right ast -> do
            let ast' = catMaybes ast
            let ast'' = if length ast > 1 then Node (Literal "begin") [List ast'] else head ast'
              in Just <$> visitAST (dropFileName file) ast''
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node (Literal "import") [String path]) = do
    let path' = path ++ if endsWith ".qrk" path then "" else ".qrk"
    path <- if startsWith "std:" path'
      then lookupEnv "QUARK" >>= \case
        Just x -> do
          let path'' = x </> "tests" </> drop 4 path' 
          exists <- doesFileExist path''
          if exists
            then return path''
            else error $ "Directory " ++ path'' ++ " does not exist"
        Nothing -> error "QUARK environment variable is not set"
      else return $ p </> path'
    return $ Node (Literal "import") [String path]

  visitAST p z@(Node (Literal "begin") [List xs]) = do
    xs <- mapM (visitAST p) xs
    return $ Node (Literal "begin") [List xs]

  visitAST _ z = return z


  convertString :: String -> AST
  convertString s = Node (Literal "list") $ map Char s
