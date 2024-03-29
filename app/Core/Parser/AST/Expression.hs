module Core.Parser.AST.Expression where
  import Core.Parser.AST.Literal
  import Core.Utility.Color (bold, bBlue)

  data Expression
    = Node Expression [Expression]
    | List [Expression]
    | Literal Literal
    | Quoted Expression
    | Identifier String
    deriving Eq

  instance Show Expression where
    show (Node e es) = "(" ++ show e ++ " " ++ unwords (map show es) ++ ")"
    show (List es) = "[" ++ unwords (map show es)  ++ "]"
    show (Literal l) = show l
    show (Quoted e) = bold "@" ++ show e
    show (Identifier s) = bBlue s