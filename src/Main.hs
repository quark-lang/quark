{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where
  import Core.Parser (parse)
  import Core.Compiler (compile, initProgram)
  import Control.Monad.State (runState)
  
  main = do
    let res = parse "(begin (let fun (fn (x) (+ 5 x))) (begin (fun 7) (fun 8)) (let x 5))"
    case res of
      Left err -> error . show $ err
      Right ast -> do
        print $ let (_, res) = runState (compile ast) initProgram in res
        -- print ast