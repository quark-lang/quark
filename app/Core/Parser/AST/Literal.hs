module Core.Parser.AST.Literal where
  import Core.Utility.Color
  data Literal
    = Integer Integer
    | String String
    | Float Float
    | Char Char
    deriving Eq

  instance Show Literal where
    show (Integer i) = bYellow $ show i
    show (String s) = bGreen $ show s
    show (Float s) = bYellow $ show s
    show (Char c) = bGreen $ show c