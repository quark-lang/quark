module Core.Compiler.Utils.Pretty where
  import System.Console.ANSI
  import System.Console.ANSI.Codes (ConsoleIntensity(BoldIntensity))
  import System.Console.ANSI.Types (Color)

  instruction :: (ColorIntensity, Color) -> String -> String
  instruction (i, c) s
    = setSGRCode [Reset, SetColor Foreground i c] 
        ++ s ++ " " ++ setSGRCode [Reset]

  argument :: (ColorIntensity, Color) -> String -> String
  argument (i, c) n
    = setSGRCode [Reset, SetColor Foreground i c] 
        ++ n ++ setSGRCode [Reset]

  bold :: String -> String
  bold s 
    = setSGRCode [Reset, SetConsoleIntensity BoldIntensity] 
        ++ s ++ setSGRCode [Reset]

  

  