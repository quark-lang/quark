{-# LANGUAGE LambdaCase #-}
module Core.Import.Duplicates where
  import Core.Import.Type
  import Core.Parser.AST
  import System.FilePath
  import Core.Parser.Parser ( parseLisp )
  import Text.Megaparsec (ParseErrorBundle)
  import Data.Void (Void)
  import Core.Import.Remover (runImportRemover)
  import System.Environment (lookupEnv)
  
  type Import = (Either (ParseErrorBundle String Void) ImportMap)

  getImportMap :: Path -> Expression -> IO Import
  getImportMap dir (Node (Identifier "import") [Literal (String path)]) = do
    path' <- case path of
      's':'t':'d':':':path -> lookupEnv "QUARK" >>= \case
        Nothing -> error "QUARK environment variable not set"
        Just quark -> return $ quark </> "library" </> path
      _ -> return $ dir </> path
    content <- readFile path'
    case parseLisp content of
      Left err -> return $ Left err
      Right ast -> do 
        x <- getImports (takeDirectory path') ast
        return $ appendPath (path', runImportRemover ast) <$> x
  getImportMap _ _ = return . pure $ []

  getImports :: Path -> [Expression] -> IO Import
  getImports dir imports = do
    imports' <- mapM (getImportMap dir) imports
    return $ foldl (\acc x -> case x of
      Left err -> Left err
      Right path -> mergePaths <$> acc <*> pure path) (Right []) imports'