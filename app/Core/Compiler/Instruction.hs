module Core.Compiler.Instruction where
  data Instruction
    -- value related
    = PUSH Int
    | POP

    -- variable related
    | STORE String
    | LOAD String
    | DROP String

    -- lambda related
    | MAKE_LAMBDA Int
    | CALL Int
    | RETURN
    | HALT
    | ENV String
    | EXTERN Int

    -- arithmetic
    | ADD
    | SUB
    | MUL
    | DIV
    deriving Show

  type Bytecode = [Instruction]