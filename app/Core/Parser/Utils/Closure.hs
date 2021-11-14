module Core.Parser.Utils.Closure where
  import Core.Parser.AST (AST(..))

  {-
    Module: Closure conversion
    Description: Converting closured functions to normal functions and putting them at the top of the scope
                 and passing their environment to the arguments 
    Author: thomasvergne
  -}

  convertClosure :: AST -> AST -> AST
  convertClosure p (Node "begin" xs) = Node "begin" (map (convertClosure p) xs)
  convertClosure p (Node "let" [name, Node "fn" (args:body)]) = error . show $ body
  convertClosure _ x = x