module Core.Import.Duplicates where
  import Core.Import.Type
  import Core.Parser.AST
  import System.FilePath
  import Core.Parser.Parser ( parseLisp )
  import Text.Megaparsec (ParseErrorBundle)
  import Data.Void (Void)
  import Core.Import.Remover (runImportRemover)
  
  type Import = (Either (ParseErrorBundle String Void) ImportMap)

  getImportMap :: Path -> Expression -> IO Import
  getImportMap dir (Node (Literal (Identifier "import")) [Literal (String path)]) = do
    let src = dir </> path
    content <- readFile src
    case parseLisp content of
      Left err -> return $ Left err
      Right ast -> do 
        x <- getImports (takeDirectory src) ast
        return $ appendPath (src, runImportRemover ast) <$> x
  getImportMap _ _ = return . pure $ []

  getImports :: Path -> [Expression] -> IO Import
  getImports dir imports = do
    imports' <- mapM (getImportMap dir) imports
    return $ foldl (\acc x -> case x of
      Left err -> Left err
      Right path -> mergePaths <$> acc <*> pure path) (Right []) imports'