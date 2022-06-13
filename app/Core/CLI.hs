module Core.CLI where
  import Core.Color
  data Option
    = String :>: String
    | Option String
    | Raw String
    deriving Show

  parseArguments :: [String] -> [Option]
  parseArguments (x:xs) = case x of
    '-':'-':opt -> case xs of
      y:ys -> opt :>: y : parseArguments ys
      [] -> parseArguments xs
    '-':y -> Option y : parseArguments xs
    _ -> Raw x : parseArguments xs
  parseArguments [] = []

  data Command
    = Command String [Option]

  parseCommand :: [String] -> Maybe Command
  parseCommand (x:xs) = Just $ Command x (parseArguments xs)
  parseCommand [] = Nothing

  alert :: String -> IO ()
  alert x = putStrLn $ bBlack "[" ++ "!" ++ bBlack "] " ++ x

