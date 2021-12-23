module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  import Core.Parser.Utils.ClosureConversion
  import Core.Compiler.Compiler
  import Core.Compiler.Javascript (compileJavascript)
  
  main :: IO ()
  main = do
    let src = "tests/main.qrk"
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        m <- runMacroCompiler ast
        g <- runGarbageCollector m
        let c = runRemover $ propagate g
        showAST 0 c
        ClosureConversion _ ast _ _ <- runClosureConverter c
        mapM_ (\(Closure n _ (t, b)) -> print (n, t, b)) ast
        

    