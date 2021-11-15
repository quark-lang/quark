module Core.Compiler.Compiler where
  import Core.Compiler.Instruction (Bytecode, Instruction(..))
  import Core.Parser.AST (AST(..))
  import Core.Parser.Utils.Garbage (toList)

  flat :: [[a]] -> [a] -> [a]
  flat xs acc = foldl (++) acc xs

  compile :: Int -> AST -> Bytecode
  compile i (Node (Literal "begin") xs)
    = let r  = foldl (\acc x -> acc ++ compile (length acc) x) [] xs
          i' = length r
        in r ++ [HALT]

  compile i (Node (Literal "let") ((Literal name):value:_))
    = compile i value ++ [STORE name]

  compile i (Node (Literal "drop") ((Literal name):_))
    = [DROP name]

  compile i (Node (Literal "print") (x:_))
    = compile i x ++ [EXTERN 0]

  compile i (Node (Literal "env") ((Literal x):_))
    = [ENV x]

  compile i (Node (Literal "chr") (x:_))
    = compile i x ++ [EXTERN 2]

  compile i (Node (Literal "return") (x:_))
    = compile i x ++ [RETURN]

  compile i (Node (Literal "if") (cond:then_:else_:_))
    = let cond' = compile (i + 1) cond
          lc = length cond'

          then_' = compile (i + lc + 1) then_
          lt = length then_'

          else_' = compile (i + lt + lc + 1) else_
          le = length else_'

          i' = i + 1
      in cond' ++ [JUMP_IF (lc + i') (lc + i' + lt + 1)] 
          ++ then_' ++ [JUMP (lc + i' + lt + le + 2)] 
          ++ else_' ++ [JUMP (lc + i' + lt + le + 2)]

  compile i (Node (Literal "fn") (args:body))
    = let args'  = map (\(Literal name) -> STORE name) (toList args)
          body'  = map (compile i) body
          body'' = concat (args' : body')
          l      = length body''
      in  MAKE_LAMBDA l : body''

  compile i (Node x xs)
    = foldl (\acc x -> acc ++ compile (length acc + i) x) (compile i x) xs ++ [CALL (length xs)]

  compile i (Literal x) = [LOAD x]
  compile _ (Integer i) = [PUSH (fromInteger i)]
  compile _ _ = []