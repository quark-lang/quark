{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Quark
  import System.FilePath ( (</>), takeDirectory )
  import qualified Data.Map as M
  import Core.Import.Duplicates (getImports)
  import Core.Import.Remover
  
  run :: (String, String) -> IO ()
  run (dir, file) = do
    let src = dir </> file
    content <- readFile src
    let ast = parseLisp content
    case ast of
      Right ast -> do
        print ast
        -- Import map
        ast' <- fmap (map snd) <$> getImports (takeDirectory src) ast
        let res = fmap concat ((++[runImportRemover ast]) <$> ast')
        case res of
          Left err -> print err
          Right ast' -> do
            let env = runEnvironments ast'
            let x = runMacroCompiler (compileMany $ runMacroRemover ast') env
            print x
      Left err -> print err