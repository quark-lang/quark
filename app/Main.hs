{-# LANGUAGE ScopedTypeVariables #-}
module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> showAST 0 ast

