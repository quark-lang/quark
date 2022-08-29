module Core.Utility.Sugar where
  import Core.Parser.AST (Expression(..), Literal (String, Char))
  import Data.List (isPrefixOf)
  import Debug.Trace (traceShow)
  
  buildCall :: Expression -> [Expression] -> Expression
  buildCall f [] = f
  buildCall call (x:args) = buildCall (Node call [x]) args

  buildClosure :: [Expression] -> Expression -> Expression
  buildClosure [] b = b
  buildClosure (x:xs) b = Node (Identifier "fn") [List [if isNode x then Identifier "$x" else x], if isNode x then Node (Identifier "match") [Identifier "$x", List [x, buildClosure xs b]] else buildClosure xs b]

  isConsCall :: Expression -> Bool
  isConsCall (Node (Identifier (n:_)) _) = n `elem` ['A'..'Z']
  isConsCall _ = False

  reserved :: Expression -> Bool
  reserved (Node (Identifier "declare") _) = True
  reserved (Node (Identifier "let") _) = True
  reserved (Node (Identifier "class") _) = True
  reserved _ = False

  isNode :: Expression -> Bool
  isNode (Node _ _) = True
  isNode _ = False

  eliminateSugar :: Expression -> Expression
  eliminateSugar (Node (Identifier "begin") xs) = buildBeginSugar xs
  eliminateSugar (Node (Identifier "do") xs) = eliminateSugar $ buildDoSugar xs
  eliminateSugar z@(Node (Identifier "import") _) = z
  eliminateSugar (Node (Identifier "instance") [i, x, List methods]) = 
    Node (Identifier "instance") [i, x, List (map eliminateSugar methods)]
  eliminateSugar (Node (Identifier "match") (pat:cases)) =
    let cases' = map (\(List [pat, expr]) -> List [eliminateSugar pat, eliminateSugar expr]) cases
      in Node (Identifier "match") (eliminateSugar pat:cases')
  eliminateSugar (Node (Identifier "fn") [List args, body])
    = if not (null args)
        then buildClosure args (eliminateSugar body)
        else eliminateSugar body
  eliminateSugar z@(Node (Identifier "data") _) = z
  eliminateSugar (Node (Identifier "let") [z@(Node _ _), value, body]) = 
    eliminateSugar (Node (Identifier "match") [value, List [z, body]]) 
  eliminateSugar (Node (Identifier "let") [name, value])
    = Node (Identifier "let") [eliminateSugar name, eliminateSugar value]
  eliminateSugar (Node (Identifier "let") [name, value, body])
    = Node (Identifier "let") [eliminateSugar name, eliminateSugar value, eliminateSugar body]
  eliminateSugar e@(Node n xs) =
    let z@(Node n' xs') = Node (eliminateSugar n) (map eliminateSugar xs)
      in if isConsCall e 
        then Node n xs' 
        else if reserved e
          then e 
          else buildCall n' xs'
  eliminateSugar (List xs) = buildList xs
  eliminateSugar (Literal (String s)) = buildString s
  eliminateSugar x = x

  buildString :: String -> Expression
  buildString [] = Identifier "Nil"
  buildString (x:xs) = Node (Identifier "Cons") [Literal (Char x), buildString xs]

  buildBeginSugar :: [Expression] -> Expression
  buildBeginSugar [x] = eliminateSugar x
  buildBeginSugar (Node (Identifier "let") [name, value]:xs)
    = eliminateSugar $ Node (Identifier "let") [
        name,
        eliminateSugar value,
        buildBeginSugar xs ]
  buildBeginSugar (x:xs)
    = Node (Identifier "let") [
        Identifier "_",
        eliminateSugar x,
        buildBeginSugar xs ]
  buildBeginSugar [] = Identifier "nil"

  buildList :: [Expression] -> Expression
  buildList [] = Identifier "Nil"
  buildList [x, Identifier y] = if "*" `isPrefixOf` y then Node (Identifier "Cons") [eliminateSugar x, Identifier $ drop 1 y] else Node (Identifier "Cons") [eliminateSugar x, Node (Identifier "Cons") [Identifier y, Identifier "Nil"]]
  buildList (x:xs) = Node (Identifier "Cons") [eliminateSugar x, buildList xs]

  buildDoSugar :: [Expression] -> Expression
  buildDoSugar [x] = eliminateSugar x
  buildDoSugar (Node (Identifier "<-") [name, value]:xs)
    = eliminateSugar $ Node (Identifier ">>=") [
        value,
        Node (Identifier "fn") [List [name], buildDoSugar xs]]
  buildDoSugar (Node (Identifier "let") [name, value]:xs)
    = eliminateSugar $ Node (Identifier "let") [
        eliminateSugar name,
        eliminateSugar value,
        buildDoSugar xs ]
  buildDoSugar (x:xs) = Node (Identifier ">>") [eliminateSugar x, buildDoSugar xs]
  buildDoSugar [] = Node (Identifier "return") [Identifier "Void"]