{-# LANGUAGE LambdaCase, TupleSections #-}
module Core.CLI.Commands.Help where
  import Core.CLI.Type
  import Core.Utility.Color

  helpCmd :: CommandExecution
  helpCmd _ i = do
    putStrLn $ bold "Quark " ++ bMagenta "0.0.1" ++ bBlack " - A functionnal programming language."
    putStrLn ""
    putStrLn $ "Usage: " ++ bBlack "quark [command] <options>"
    putStrLn "Commands:"
    mapM_ (\(n,c) -> putStrLn $ "  " ++ bBlack n ++ " - " ++ c) i

  help :: (String, (String, CommandExecution))
  help = ("help", ("Prints this help message", helpCmd))
