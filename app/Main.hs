{-# LANGUAGE ScopedTypeVariables #-}
module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (garbageCollection)
  import Core.Parser.Utils.Closure (convertClosure)
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        let r = garbageCollection ast
        showAST 0 (convertClosure r r)

