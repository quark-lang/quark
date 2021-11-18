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

  import Foreign.C.Types
  import Foreign.Ptr
  import Foreign.StablePtr ()
  import Foreign.C.String
  import Foreign.Marshal.Array
  import Core.Parser.Utils.ConstantPropagation
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
    let src = "tests/path.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        let r = garbageCollection ast
        let d = Data []
        let c = convertClosure (r, d) r
        -- showAST 0 c
        let res = compile 0 c
        showBytecode res
        let Serialized blob constants = runSerializer res
        print blob

        blob' <- convertBlob blob
        constants' <- convertConstants constants

        x <- runVM blob' (fromIntegral $ length blob) constants' (fromIntegral $ length constants)
        print x

    