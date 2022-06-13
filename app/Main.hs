module Main where
  import System.Environment (getArgs)
  import System.Directory
  import System.FilePath ((</>))
  import Core.Entry (run)
  import Core.CLI
  import Core.Color

  help :: IO ()
  help = do
    putStrLn $ bold "Quark " ++ bMagenta "0.0.1" ++ bBlack " - A functionnal programming language."
    putStrLn ""
    putStrLn $ "Usage: " ++ bBlack "quark [options]"
    putStrLn "Commands:"
    putStrLn $ "  " ++ bBlack "help" ++ "  => Show this help."
    putStrLn $ "  " ++ bBlack "build" ++ " => Build quark file."

  build :: String -> IO ()
  build x = do
    dir <- getCurrentDirectory
    run (dir, x)

  main :: IO ()
  main = do
    x <- parseCommand <$> getArgs
    case x of
      Just (Command name opts) ->
        case name of
          "build" -> case opts of
            [] -> alert "No target specified!"
            (Raw file:_) -> build file
          "help" -> help
          x -> build x
      Nothing -> do
        putStrLn $ bold "Quark " ++ bMagenta "0.0.1" ++ bBlack " - A functionnal programming language."
        putStrLn $ "Type " ++ bMagenta "quark help" ++ " to start."