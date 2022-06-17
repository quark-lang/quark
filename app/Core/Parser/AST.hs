module Core.Parser.AST where
  import Data.List
  import Core.Color
  data AST
    = Node AST [AST]
    | Integer Integer
    | String String
    | Float Float
    | Literal String
    | Char Char
    | List [AST]
    deriving Eq

  instance Show AST where
    show (Node a b) = "(" ++ show a ++ " " ++ intercalate " " (map show b) ++ ")"
    show (Integer i) = bYellow $ show i
    show (String s) = bGreen $ show s
    show (Float f) = bYellow $ show f
    show (Literal s) = bBlue s
    show (Char c) = bGreen $ show c
    show (List l) = bBlack "[" ++  intercalate " " (map show l) ++ bBlack "]"