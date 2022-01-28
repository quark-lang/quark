module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  import Core.Parser.Utils.ClosureConversion (runConverter, closures)
  import Core.Parser.TypeDeducer (runDeducer)
  import Core.Compiler.CLang (runCompiler)

  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        m <- runMacroCompiler ast
        -- removing useless scope related things
        g <- runGarbageCollector m
        -- propagating constants and removing useless code
        let r = runRemover $ propagate g
        -- creating a typed AST
        x <- runDeducer r
        print x
        -- converting closures
        t <- runConverter x
        c <- runCompiler $ closures t
        print c