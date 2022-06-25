module Core.Parser.AST.Literal where
  import Core.Color
  data Literal
    = Integer Integer
    | String String
    | Float String
    | Identifier String
    deriving Eq

  instance Show Literal where
    show (Integer i) = bYellow $ show i
    show (String s) = bGreen $ show s
    show (Float s) = bYellow $ show s
    show (Identifier s) = bBlue s