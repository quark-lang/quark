{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (garbageCollection)
  import Core.Parser.Utils.Closure (convertClosure, Data(..))
  import Core.Compiler.Compiler (compile)
  import Core.Compiler.Utils.Pretty (showBytecode)
  import Core.Compiler.Serializer
  import Data.List
  import Core.Compiler.Instruction
  import Core.Parser.AST

  import Foreign.C.Types
  import Foreign.Ptr
  import Foreign.StablePtr ()
  import Foreign.C.String
  import Foreign.Marshal.Array
  foreign import capi "Core/VM/main.c runVM"
    runVM :: Ptr (Ptr CInt) -> CInt -> Ptr (Ptr CChar) -> CInt -> IO CInt

  convertBlob :: [[Int]] -> IO (Ptr (Ptr CInt))
  convertBlob xs = do
    ys <- mapM (\x -> do
      newArray (map (CInt . fromIntegral) x)) xs
    newArray ys

  convertConstants :: [String] -> IO (Ptr (Ptr CChar))
  convertConstants xs = do
    ys <- mapM newCString xs
    newArray ys
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        let c = convertClosure ast
        showAST 0 c
        let g = garbageCollection ast
        let e = compile 0 (case g of
                  Node (Literal "begin") _ -> g
                  _ -> Node (Literal "begin") [g])

        print e

        outputBytecode "bytecode.bin" e
    
  outputBytecode :: String -> Bytecode -> IO ()
  outputBytecode src b = do
    let content = unlines $ map show b
    writeFile src content