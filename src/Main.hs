{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where
  import Core.Parser (parse)
  import Core.Compiler
  import Control.Monad.State (runState)
  import System.Console.Pretty

  colorizeValue :: Value -> String
  colorizeValue (VInteger i) = color Yellow (show i)
  colorizeValue (VString str) = color Green (show str)

  colorizeInstruction :: Bytecode -> String
  colorizeInstruction (LOAD_SEGMENT seg) = style Bold " LOAD_SEGMENT " ++ color Yellow (show seg)
  colorizeInstruction (PUSH val)         = style Bold " PUSH " ++ colorizeValue val
  colorizeInstruction (CALL i)           = style Bold " CALL " ++ color Yellow (show i)
  colorizeInstruction (STORE x)          = style Bold " STORE " ++ color Blue x
  colorizeInstruction (LOAD x)           = style Bold " LOAD " ++ color Blue x


  printProgram :: [Page] -> IO()
  printProgram bc = do
    let indexed = zip [0..length bc - 1] bc
    mapM_ (\(i, b) -> do
      putStrLn $ color Yellow (show i ++ ":")
      mapM_ (putStrLn . colorizeInstruction) b
      putStrLn ""
      ) indexed

  main = do
    let res = parse "((fn (x) (x (+ 5 2))) 7 \"test\")"
    case res of
      Left err -> error . show $ err
      Right ast -> do
        let (_, res) = runState (compile ast) initProgram
        let (_, prgm) = res
        printProgram prgm
        -- print ast