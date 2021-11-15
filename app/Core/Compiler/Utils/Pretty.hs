module Core.Compiler.Utils.Pretty where
  import System.Console.ANSI
  import Core.Compiler.Instruction (Instruction(..), Bytecode)
  import System.Console.ANSI.Codes (ConsoleIntensity(BoldIntensity))
  import System.Console.ANSI.Types (Color)
  
  showBytecode :: Bytecode -> IO ()
  showBytecode = mapM_ showInstruction

  instruction :: Color -> String -> String
  instruction c s
    = setSGRCode [Reset, SetColor Foreground Vivid c] 
        ++ s ++ " " ++ setSGRCode [Reset]

  bold :: String -> String
  bold s 
    = setSGRCode [Reset, SetConsoleIntensity BoldIntensity] 
        ++ s ++ setSGRCode [Reset]

  showInstruction :: Instruction -> IO ()

  -- value related
  showInstruction (PUSH n) 
    = putStr (instruction Green "PUSH       ") >> putStrLn (bold . show $ n)
  showInstruction POP = putStrLn (instruction Red "POP        ")

  -- arithmetic
  showInstruction ADD = putStrLn (instruction Black "ADD        ")
  showInstruction SUB = putStrLn (instruction Black "SUB        ")
  showInstruction MUL = putStrLn (instruction Black "MUL        ")
  showInstruction DIV = putStrLn (instruction Black "DIV        ")

  -- miscellaneous
  showInstruction HALT = putStrLn (instruction Red "HALT       ")
  showInstruction (JUMP i) 
    = putStr (instruction Yellow "JUMP       ") >> putStrLn (bold . show $ i)
  showInstruction (JUMP_IF a b)
    = putStr (instruction Yellow "JUMP_IF    ") 
        >> putStr (bold . show $ a) 
        >> putStr " "
        >> putStrLn (bold . show $ b)
  showInstruction (EXTERN i)
    = putStr (instruction Black "EXTERN     ") >> putStrLn (bold . show $ i)
  
  -- variable related
  showInstruction (STORE x) 
    = putStr (instruction Blue "STORE      ") >> putStrLn (bold x)
  showInstruction (LOAD x)
    = putStr (instruction Blue "LOAD       ") >> putStrLn (bold x)
  showInstruction (DROP x)
    = putStr (instruction Blue "DROP       ") >> putStrLn (bold x)
  
  -- lambda related
  showInstruction (MAKE_LAMBDA x)
    = putStr (instruction Green "MAKE_LAMBDA") >> putStrLn (bold . show $ x)
  showInstruction (CALL x)
    = putStr (instruction Cyan  "CALL       ") >> putStrLn (bold . show $ x)
  showInstruction RETURN = putStrLn (instruction Black "RETURN     ")
  showInstruction (ENV s)
    = putStr (instruction Black "ENV        ") >> putStrLn (bold s)

  