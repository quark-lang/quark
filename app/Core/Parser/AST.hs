module Core.Parser.AST where
  data AST
    = Node AST [AST]
    | Integer Integer
    | String String
    | Float Float
    | Literal String
    | Char Char
    deriving (Show, Eq)