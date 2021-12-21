module Core.Parser.Utils.ConstantPropagation where
  import Core.Parser.AST (AST(..))

  {-
    Module: Constant propagation
    Description: Containing functions for propagating constants through AST in order to reduce it
    Author: thomasvergne
  -}
  
  propagate :: AST -> AST
  propagate z@(Node (Literal n) [x, y]) = 
    let n1 = propagate x
        n2 = propagate y
      in case (n1, n2) of
        (Float x, Float y) -> case n of
          "+" -> Float (x + y)
          "-" -> Float (x - y)
          "*" -> Float (x * y)
          "/" -> Float (x / y)
          _ -> Node (Literal n) [n1, n2]
        _ -> Node (Literal n) [n1, n2]

  propagate (Node n xs) = Node (propagate n) (map propagate xs)
  propagate (Integer n) = Float $ fromIntegral n
  propagate n = n
  