module Core.Utility.Color where
  bold :: String -> String
  bold s = "\x1b[1m" ++ s ++ "\x1b[0m"

  underline :: String -> String
  underline s = "\x1b[4m" ++ s ++ "\x1b[0m"

  black :: String -> String
  black s = "\x1b[30m" ++ s ++ "\x1b[0m"

  red :: String -> String
  red s = "\x1b[31m" ++ s ++ "\x1b[0m"

  green :: String -> String
  green s = "\x1b[32m" ++ s ++ "\x1b[0m"

  yellow :: String -> String
  yellow s = "\x1b[33m" ++ s ++ "\x1b[0m"

  blue :: String -> String
  blue s = "\x1b[34m" ++ s ++ "\x1b[0m"

  magenta :: String -> String
  magenta s = "\x1b[35m" ++ s ++ "\x1b[0m"

  cyan :: String -> String
  cyan s = "\x1b[36m" ++ s ++ "\x1b[0m"

  white :: String -> String
  white s = "\x1b[37m" ++ s ++ "\x1b[0m"

  bBlack :: String -> String
  bBlack s = "\x1b[90m" ++ s ++ "\x1b[0m"

  bRed :: String -> String
  bRed s = "\x1b[91m" ++ s ++ "\x1b[0m"

  bGreen :: String -> String
  bGreen s = "\x1b[92m" ++ s ++ "\x1b[0m"

  bYellow :: String -> String
  bYellow s = "\x1b[93m" ++ s ++ "\x1b[0m"

  bBlue :: String -> String
  bBlue s = "\x1b[94m" ++ s ++ "\x1b[0m"

  bMagenta :: String -> String
  bMagenta s = "\x1b[95m" ++ s ++ "\x1b[0m"

  bCyan :: String -> String
  bCyan s = "\x1b[96m" ++ s ++ "\x1b[0m"