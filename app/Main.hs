{-# LANGUAGE LambdaCase #-}
module Main where
  import System.Directory (getCurrentDirectory)
  import Core.Entry (run)
  import System.FilePath
  import Core.Utility.Error (printError)
  
  build :: String -> IO ()
  build x = do
    dir <- getCurrentDirectory
    run (dir, x) >>= \case
      Left err -> printError err
      Right js -> do
        writeFile (dir </> x -<.> "cpp") js

  main :: IO ()
  main = build "tests/facto.qrk"