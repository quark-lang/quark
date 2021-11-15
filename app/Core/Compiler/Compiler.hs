module Core.Compiler.Compiler where
  import Core.Compiler.Instruction (Bytecode, Instruction(..))
  import Core.Parser.AST (AST(..))
  import Core.Parser.Utils.Garbage (toList)

  flat :: [[a]] -> [a] -> [a]
  flat xs acc = foldl (++) acc xs

  compile :: AST -> Bytecode
  compile (Node (Literal "begin") xs)
    = foldl (\acc x -> acc ++ compile x) [] xs

  compile (Node (Literal "let") ((Literal name):value:_))
    = compile value ++ [STORE name]

  compile (Node (Literal "drop") ((Literal name):_))
    = [DROP name]

  compile (Node (Literal "print") (x:_))
    = compile x ++ [EXTERN 0]

  compile (Node (Literal "env") ((Literal x):_))
    = [ENV x]

  compile (Node (Literal "chr") (x:_))
    = compile x ++ [EXTERN 2]

  compile (Node (Literal "return") (x:_))
    = compile x ++ [RETURN]

  compile (Node (Literal "fn") (args:body))
    = let args'  = map (\(Literal name) -> STORE name) (toList args)
          body'  = map compile body
          body'' = concat (args' : body')
          l      = length body''
      in  MAKE_LAMBDA l : body''

  compile (Node x xs)
    = foldl (\acc x -> acc ++ compile x) (compile x) xs ++ [CALL (length xs)]

  compile (Literal x) = [LOAD x]
  compile (Integer i) = [PUSH (fromInteger i)]
  compile _ = []