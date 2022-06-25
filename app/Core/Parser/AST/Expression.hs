module Core.Parser.AST.Expression where
  import Core.Parser.AST.Literal
  import Core.Color (bold)

  data Expression
    = Node Expression [Expression]
    | List [Expression]
    | Literal Literal
    | Quoted Expression
    deriving Eq

  instance Show Expression where
    show (Node e es) = "(" ++ show e ++ " " ++ unwords (map show es) ++ ")"
    show (List es) = "[" ++ unwords (map show es)  ++ "]"
    show (Literal l) = show l
    show (Quoted e) = bold "@" ++ show e