{-# LANGUAGE TupleSections #-}
module Core.CLI.CLI where
  import Core.Utility.Color
  import Core.Utility.Error (printError)
  import Core.CLI.Type
  import System.Environment
  import Control.Arrow (Arrow(second))

  parseArguments :: [String] -> [Option]
  parseArguments (x:xs) = case x of
    '-':'-':opt -> case xs of
      y:ys -> opt :>: y : parseArguments ys
      [] -> parseArguments xs
    '-':y -> Option y : parseArguments xs
    _ -> Raw x : parseArguments xs
  parseArguments [] = []

  parseCommand :: [String] -> Maybe Command
  parseCommand (x:xs) = Just $ Command x (parseArguments xs)
  parseCommand [] = Nothing

  alert :: String -> IO ()
  alert = printError . (,Nothing)

  contains :: String -> [Option] -> Bool
  contains x = any ((== x) . optionName)

  optionName :: Option -> String
  optionName (Option x) = x
  optionName (Raw x) = x
  optionName (x :>: _) = x

  isRaw :: Option -> Bool
  isRaw (Raw _) = True
  isRaw _ = False

  isOption :: Option -> Bool
  isOption (Option _) = True
  isOption _ = False

  isArgument :: Option -> Bool
  isArgument (_ :>: _) = True
  isArgument _ = False

  runCLI :: Commands -> IO ()
  runCLI c = do
    x <- parseCommand <$> getArgs
    case x of
      Just (Command name args) -> do
        case lookup name c of
          Just (_, f) ->
            let infos = map (second fst) c
              in f args infos
          Nothing -> alert $ "Command " ++ bold name ++ " not found"
      Nothing -> alert "No command given"