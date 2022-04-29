module Main where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  --import Core.Parser.Utils.ClosureConversion -- (runConverter, closures)
  --import Core.Compiler.CLang (runCompiler, outputC)
  import Core.Inference.Type (runInfer)

  import System.Environment (getArgs)
  import System.Directory (getCurrentDirectory)
  import System.FilePath ((</>))
  
  main :: IO ()
  main = do
    (file:_) <- getArgs
    dir <- getCurrentDirectory
    let src = dir </> file
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        --m <- runMacroCompiler ast
        -- removing useless scope related things
        --g <- runGarbageCollector m
        -- propagating constants and removing useless code
        --let r = runRemover $ propagate g
        -- creating a typed AST
        -- converting closures
        -- print ast
        t <- runInfer ast
        print t
        --c <- convertClosures t
        --mapM_ print c
        -- c <- runCompiler $ closures t
        -- writeFile (dir </> (file -<.> "c")) (outputC c)
