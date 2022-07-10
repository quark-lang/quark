module Core.CLI.Type where
  data Option
    = String :>: String
    | Option String
    | Raw String
    deriving Show

  data Command
    = Command String [Option]

  type Commands = [(String, (String, CommandExecution))]
  type CommandInformation = (String, String)
  type CommandExecution   = [Option] -> [CommandInformation] -> IO ()
