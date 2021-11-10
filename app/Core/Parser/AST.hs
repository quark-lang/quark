module Core.Parser.AST where
  data AST
    = Node String [AST]
    | Integer Integer
    | String String
    | Float Float
    | Literal String
    deriving (Show, Eq)