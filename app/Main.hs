module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Pretty (showAST)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  import Core.Parser.Utils.ClosureConversion
  import Core.Compiler.Compiler
  import Core.Parser.Utils.ClosureConversion (ClosuredAST)
  import Data.List
  
  sortClosures :: ClosuredAST -> ClosuredAST
  sortClosures = sortBy (\(Closure a _ _) (Closure b _ _) -> compare a b)

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
        ClosureConversion _ ast _ _ <- runClosureConverter c
        let ast' = sortClosures ast
        mapM_ (putStrLn . ("\n"++) . show) ast'
        section <- compile ast'
        putStrLn ""
        mapM_ print section
        

    