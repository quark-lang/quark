{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  import Core.Parser.Utils.ClosureConversion -- (runConverter, closures)
  import Core.Compiler.CLang (runCompiler, formatProgram)
  import Core.Inference.Type (runInfer)
  import Core.Parser.Utils.Imports

  import System.Environment (getArgs)
  import System.Directory
  import System.Process
  import System.FilePath ((</>), (-<.>))
  import Data.Bifunctor (Bifunctor(first))
  import Data.Foldable (foldlM)

  import Core.Color
  import qualified Data.Map as M

  step :: (Int, Int) -> String
  step (i, f) = bBlack "[" ++ show i ++ "/" ++ show f ++ bBlack "]"

  compile :: String -> (String, String) -> String -> IO ()
  compile compiler (dir, source) c = do
    let src = source -<.> "cpp"
    let cppOutput = dir </> src
    let exeOutput = cppOutput -<.> ""

    putStrLn $ step (3, 5) ++  " Compiling " ++ bMagenta source ++ " to " ++ bMagenta src
    writeFile cppOutput c
    putStrLn $ step (4, 5) ++ " Outputing executable.."
    callCommand $ compiler ++ " -std=c++14 -O3 -Wno-all " ++ cppOutput ++ " -o" ++ exeOutput
    putStrLn $ step (5, 5) ++ " " ++ bMagenta source ++ " has been compiled to " ++ bMagenta src -<.> ""

  run :: (String, String) -> IO ()
  run (dir, file) = do
    let src = dir </> file
    res <- parse src
    case res of
      Nothing -> print "ERROR"
      Just ast -> do
        ast <- resolve ast
        m <- runMacroCompiler ast
        -- propagating constants and removing useless code
        let r = runRemover $ propagate ast
        -- creating a typed AST
        putStrLn $ step (1, 5) ++ " Typechecking " ++ bMagenta file ++ ".."
        t <- runInfer r
        --(closures, ast, _) <- foldlM (\(cls, acc, i) x -> do
        --  (cls', acc', i') <- convertClosures x i
        --  return (cls ++ cls', acc ++ [acc'], i')) ([], [], 0) t

        --mapM_ print ast
        --print closures
        putStrLn $ step (2, 5) ++  " Building and compiling program.."
        c <- formatProgram . fst <$> foldlM (\(res, f) x -> do
          (output, f') <- runCompiler x f
          return (res ++ [output], f')) ([], M.empty) t

        findExecutable "g++"  >>= \case
          Nothing -> findExecutable "clang++" >>= \case
            Nothing -> putStrLn "No compiler found"
            Just g -> compile "clang++" (dir, file) c
          Just _ -> compile "g++" (dir, file) c