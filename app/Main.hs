{-# LANGUAGE ScopedTypeVariables #-}
module Main where
  import Core.Parser.Utils.Module (parse, garbageCollection, removeOne)
  import Core.Parser.Utils.Pretty (showAST)
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        let r = garbageCollection ast
        showAST 0 r

