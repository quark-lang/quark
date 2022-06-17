module Core.Parser.Utils.Curry where
  import Core.Parser.AST (AST(..))
  import Prelude hiding (curry)
  import Debug.Trace

  buildCall :: AST -> [AST] -> AST
  buildCall f [] = f
  buildCall call (x:args) = buildCall (Node call [x]) args

  buildClosure :: [AST] -> AST -> AST
  buildClosure [] b = b
  buildClosure (x:xs) b = Node (Literal "fn") [x, buildClosure xs b]

  reserved = map Literal [
    "begin", "fn", "chr", "let", "declare",
    "->", "match", "data", "if"
    ]

  buildList :: [AST] -> AST
  buildList [] = Node (Literal "Nil") []
  buildList (x:xs) = Node (Literal "Cons") [x, buildList xs]

  curry :: AST -> AST
  curry (Node (Literal "fn") (List args:body:_)) = buildClosure args $ curry body
  curry (Node (Literal "begin") [List xs]) = Node (Literal "begin") [List $ map curry xs]
  curry z@(Node (Literal "declare") _) = z
  curry (Node (Literal "match") (x:pats))
    = let x'    = curry x
          pats' = (map (\(List [pat, b]) -> List [curry pat, curry b])) pats
        in Node (Literal "match") (x':pats')
  curry z@(Node (Literal "data") [_, _]) = z
  curry (Node n xs)
    = let xs' = map curry xs
        in if n `elem` reserved then Node n xs' else buildCall (curry n) xs'
  curry (List xs) = curry $ buildList xs
  curry z = z