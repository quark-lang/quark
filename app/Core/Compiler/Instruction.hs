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
    | ENV String

    -- arithmetic
    | ADD
    | SUB
    | MUL
    | DIV

    -- miscellaneous
    | HALT
    | EXTERN Int
    | JUMP Int     -- absolute jump
    | JUMP_REL Int -- relative jump
    | JUMP_ELSE Int  -- relative jump if true with then length
    deriving Show

  type Bytecode = [Instruction]