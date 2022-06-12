module Main where
  import System.Environment (getArgs)
  import System.Directory
  import System.FilePath ((</>))
  import Core.Entry (run)

  main :: IO ()
  main = do
    file:_ <- getArgs
    dir <- getCurrentDirectory
    run (dir, file)