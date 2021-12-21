module Core.Parser.Utils.ConstantPropagation where
  import Core.Parser.AST (AST(..))

  {-
    Module: Constant propagation
    Description: Containing functions for propagating constants through AST in order to reduce it
    Author: thomasvergne
  -}
  
  propagate :: AST -> AST
  -- folding condition branches
  propagate (Node (Literal "if") [cond, then', else']) =
    let cond' = propagate cond
      in if cond' == Float 1.0
        then propagate then'
        else propagate else'

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
  