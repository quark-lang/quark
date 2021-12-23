module Core.Parser.Utils.ConstantPropagation where
  import Core.Parser.AST (AST(..))
  import Core.Parser.Macros
  
  {-
    Module: Constant propagation
    Description: Containing functions for propagating constants through AST in order to reduce it
    Author: thomasvergne
  -}
  
  propagate :: AST -> AST
  -- folding condition branches
  propagate z@(Node (Literal "if") [cond, then', else']) =
    let cond' = propagate cond
      in case cond' of
        Float f -> if f == 1.0
          then propagate then'
          else propagate else'
        _ -> z

  -- folding some basic operations
  propagate z@(Node (Literal n) [x, y]) = 
    let n1 = propagate x
        n2 = propagate y
      in case (n1, n2) of
        (Float x, Float y) -> case n of
          "+" -> Float (x + y)
          "-" -> Float (x - y)
          "*" -> Float (x * y)
          "/" -> Float (x / y)
          ">" -> Float . fromIntegral . fromEnum $ x > y
          "=" -> Float . fromIntegral . fromEnum $ x == y
          "or" -> Float . fromIntegral . fromEnum $ x == 1.0 || y == 1.0
          "and" -> Float . fromIntegral . fromEnum $ x == 1.0 && y == 1.0
          _ -> Node (Literal n) [n1, n2]
        _ -> Node (Literal n) [n1, n2]

  propagate (Node n xs) = Node (propagate n) (map propagate xs)
  -- treating all the data as Float in order to simplify processing
  propagate (Integer n) = Float $ fromIntegral n
  propagate n = n
  
  -- detect if node contains declaration or impure call
  checkForDeletion :: AST -> Bool
  checkForDeletion (Node (Literal "let") _) = False
  checkForDeletion (Node (Literal "print") _) = False
  checkForDeletion (Node (Literal "input") _) = False
  checkForDeletion (Node n xs) 
    = (isPure n && checkForDeletion n) || any checkForDeletion xs
  checkForDeletion _ = True
  
  -- remove eventual useless scopes remaining after constant propagation
  removeUselessScopes :: AST -> AST
  removeUselessScopes z@(Node (Literal "begin") xs)
    = if all checkForDeletion xs
        then last xs
        else z
  removeUselessScopes (Node (Literal "spread") [x]) = removeUselessScopes x
  removeUselessScopes (Node n xs) = 
    Node (removeUselessScopes n) (map removeUselessScopes xs)
  removeUselessScopes n = n

  runRemover :: AST -> AST
  runRemover (Node (Literal "begin") xs) 
    = Node (Literal "begin") $ map removeUselessScopes xs
  runRemover z = removeUselessScopes z