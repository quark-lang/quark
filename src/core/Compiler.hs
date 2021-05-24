{-# LANGUAGE LambdaCase #-}
module Core.Compiler where
  import Core.Parser
  data Value
    = VString String
    | VInteger Integer
    | VDouble Double
    | VList [Value]
    | VLambda [Bytecode]
    deriving Show
  data Bytecode
    = PUSH Value
    | STORE String
    | LOAD String
    | LOAD_SEGMENT Integer
    | PRINT
    | CALL Int
    deriving Show

  compileDefinition :: [Atom] -> [Bytecode]
  compileDefinition ((Word name):value:_) = compile value ++ [STORE name]

  compilePrint :: [Atom] -> [Bytecode]
  compilePrint = concatMap compile

  compileLambda :: [Atom] -> [Bytecode]
  compileLambda (args:body:_) = compile body

  segment = 1

  compile :: Atom -> [Bytecode]
  compile (Expression (x:xs)) = case x of
    Word "let" -> compileDefinition xs
    Word "print" -> compilePrint xs ++ [PRINT]
    Word "begin" -> concatMap compile xs
    Word "fn" -> compileLambda xs
    Word x -> [LOAD x] ++ concatMap compile xs ++ [CALL (length xs)]

  compile (String str) = [PUSH (VString str)]
  compile (Integer int) = [PUSH (VInteger int)]
  compile (Word wrd) = [LOAD wrd]