module Core.Utility.Sugar where
  import Core.Parser.AST (Expression(..))

  eliminateSugar :: Expression -> Expression
  eliminateSugar (Node (Identifier "begin") xs) = buildBeginSugar xs
  eliminateSugar (Node (Identifier "match") (pat:cases)) =
    let cases' = map (\(List [pat, expr]) -> List [eliminateSugar pat, eliminateSugar expr]) cases
      in Node (Identifier "match") (pat:cases')
  eliminateSugar (Node (Identifier "fn") [args, body]) = Node (Identifier "fn") [args, eliminateSugar body]
  eliminateSugar z@(Node (Identifier "data") _) = z
  eliminateSugar (Node n xs) = Node (eliminateSugar n) (map eliminateSugar xs)
  eliminateSugar (List xs) = buildList xs
  eliminateSugar x = x
  
  buildBeginSugar :: [Expression] -> Expression
  buildBeginSugar [x] = x
  buildBeginSugar (Node (Identifier "let") [Identifier name, value]:y:xs) = Node (Identifier "let") [Identifier name, value, buildBeginSugar (y:xs)]
  buildBeginSugar (x:xs) = Node (Identifier "let") [Identifier "_", x, buildBeginSugar xs]
  buildBeginSugar [] = Identifier "nil"

  buildList :: [Expression] -> Expression
  buildList [] = Identifier "Nil"
  buildList (x:xs) = Node (Identifier "Cons") [eliminateSugar x, buildList xs]