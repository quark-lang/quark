{-# LANGUAGE ScopedTypeVariables #-}
module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (garbageCollection)
  import Core.Parser.Utils.Closure (convertClosure, Data(..))
  import Core.Compiler.Compiler (compile)
  import Core.Compiler.Utils.Pretty
  
  
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
        showAST 0 c
        let res = compile 0 c
        showBytecode res


