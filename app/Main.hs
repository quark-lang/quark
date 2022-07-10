module Main where
  import Core.CLI.CLI (runCLI)
  import Core.CLI.Commands.Help (help)
  import Core.CLI.Commands.Build (build)
  
  main :: IO ()
  main = runCLI [help, build]