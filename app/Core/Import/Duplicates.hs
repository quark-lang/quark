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
  import Data.List (union, find)
  import Control.Monad.RWS (MonadState (put))
  import Control.Monad.Except
  import Control.Monad.State
  
  type Import m = (MonadState [String] m, MonadIO m, MonadError (ParseErrorBundle String Void) m)
  
  addPath :: Import m => Path -> m ()
  addPath p = modify (p:)

  getImportMap :: Import m => Path -> Expression -> m [Expression]
  getImportMap dir (Node (Identifier "import") [Literal (String path)]) = do
    path' <- case path of
      's':'t':'d':':':path -> liftIO $ lookupEnv "QUARK" >>= \case
        Nothing -> error "QUARK environment variable not set"
        Just quark -> return $ quark </> "library" </> path
      _ -> return $ dir </> path
    content <- liftIO $ readFile path'
    addPath path'
    case parseLisp content of
      Left err -> throwError err
      Right ast -> getImports (takeDirectory path') ast
  getImportMap _ x = return [x]

  getImports :: Import m => Path -> [Expression] -> m [Expression]
  getImports p = (concat <$>) . mapM (getImportMap p)

  runImport :: MonadIO m => Path -> [Expression] -> m (Either (ParseErrorBundle String Void) [Expression])
  runImport dir e = runExceptT $ evalStateT (getImports dir e) []