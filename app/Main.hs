module Main where
  import System.Directory ( getCurrentDirectory )
  import Core.Entry ( run )

  build :: String -> IO ()
  build x = do
    dir <- getCurrentDirectory
    run (dir, x)

  main :: IO ()
  main = build "tests/facto.qrk"