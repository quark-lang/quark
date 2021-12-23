module Core.Compiler.Instruction where
  data Instruction
    -- value related
    = PUSH Int
    | POP

    -- variable related
    | STORE Int
    | LOAD Int
    | LOAD_SECTION Int
    | DROP Int

    -- lambda related
    | MAKE_LAMBDA Int
    | CALL Int
    | RETURN

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

  -- storing section ID and instructions
  data Section = Section Int [Instruction]
    deriving Show
