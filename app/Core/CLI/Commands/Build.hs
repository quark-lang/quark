{-# LANGUAGE LambdaCase #-}
module Core.CLI.Commands.Build where
  import Core.CLI.Type
  import Core.Utility.Color
  import System.Directory (getCurrentDirectory)
  import Core.Entry (run)
  import System.FilePath
  import Core.Utility.Error (printError)
  import Data.List
  import Core.CLI.CLI

  buildCmd :: CommandExecution
  buildCmd i _ = do
    dir <- getCurrentDirectory
    case find isRaw i of
      Just (Raw file) -> do
        run (dir, file) >>= \case
          Left err -> printError err
          Right js -> do
            writeFile (dir </> file -<.> "js") js
      Just _ -> alert "No file specified"
      Nothing -> alert "No file specified!"

  build :: (String, (String, CommandExecution))
  build = ("build", ("Compile a Quark program", buildCmd))