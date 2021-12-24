module Core.Compiler.Instruction where
  import Core.Compiler.Utils.Pretty (instruction, argument)
  import System.Console.ANSI (Color(..), ColorIntensity (..))
  data Instruction
    -- value related
    = PUSH Int
    | POP

    -- variable related
    -- loading and storing is done by specifying the variable pointer
    | STORE Int
    | LOAD Int
    | LOAD_SECTION Int
    | LOAD_CLOSURE Int [Int] -- load a closure section and its environment
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
    | EXTERN Int -- external function call like print or input
    | JUMP Int -- absolute jump
    | JUMP_REL Int -- relative jump
    | JUMP_ELSE Int -- relative jump if true with then length

  -- storing section ID and instructions
  data Section = Section Int [Instruction]

  ic :: String -> String
  ic = instruction (Dull, Green)
  
  section :: String -> String
  section = argument (Vivid, Black)

  instance Show Instruction where
    show (PUSH n) = ic "PUSH" ++ show n
    show POP = ic "POP"
    show (STORE n) = ic "STORE" ++ show n
    show (LOAD n) = ic "LOAD" ++ show n
    show (LOAD_SECTION n) = ic "LOAD_SECTION" ++ section (".section" ++ show n)
    show (LOAD_CLOSURE n ns) = ic "LOAD_CLOSURE"
      ++ section (".section" ++ show n) ++ " " ++ unwords (map show ns)
    show (DROP n) = ic "DROP" ++ show n
    show (MAKE_LAMBDA n) = ic "MAKE_LAMBDA" ++ show n
    show (CALL n) = ic "CALL" ++ show n
    show RETURN = ic "RETURN"
    show ADD = ic "ADD"
    show SUB = ic "SUB"
    show MUL = ic "MUL"
    show DIV = ic "DIV"
    show HALT = ic "HALT"
    show (EXTERN n) = ic "EXTERN" ++ show n
    show (JUMP n) = ic "JUMP" ++ show n
    show (JUMP_REL n) = ic "JUMP_REL" ++ show n
    show (JUMP_ELSE n) = ic "JUMP_ELSE" ++ show n
    
  instance Show Section where
    show (Section n is) =
      section $ ".section" ++ show n ++ ":" ++ "\n"
      ++ unlines (map (("  "++) . show) is)