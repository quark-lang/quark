{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where
  import Core.Parser (parse)
  import Core.Compiler (compile)
  
  main = do
    let res = parse "(begin (let t (fn () (print \"test\"))) (t))"
    case res of
      Left err -> print err
      Right ast -> print $ compile ast