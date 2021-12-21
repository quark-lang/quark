module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation ( propagate )
  import Core.Parser.Macros (runMacroCompiler)
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        m <- runMacroCompiler ast
        let c = propagate m
        g <- runGarbageCollector c
        showAST 0 g

    