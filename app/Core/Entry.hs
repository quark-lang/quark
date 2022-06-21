{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Parser.Utils.Module (parse)
  import Core.Parser.Utils.Garbage (runGarbageCollector)
  import Core.Parser.Utils.ConstantPropagation (propagate, runRemover)
  import Core.Parser.Macros (runMacroCompiler)
  --import Core.Parser.Utils.ClosureConversion -- (runConverter, closures)
  import Core.Compiler.Javascript (runCompiler, from)
  import Core.Inference.Type (runInfer)
  import Core.Parser.Utils.Imports
  import Prelude hiding (curry)
  import Core.Parser.Utils.Curry

  import System.Environment (getArgs)
  import System.Directory
  import System.Process
  import System.FilePath ((</>), (-<.>))
  import Data.Bifunctor (Bifunctor(first))
  import Data.Foldable (foldlM)

  import Core.Color
  import qualified Data.Map as M
  import qualified Core.Compiler.Uncurry as U
  import Data.List

  compile :: (String, String) -> String -> IO ()
  compile (dir, source) c = do
    let src = source -<.> "js"
    let cppOutput = dir </> src

    writeFile cppOutput c
    putStrLn $ bMagenta source ++ " has been compiled to " ++ bMagenta src

  run :: (String, String) -> IO ()
  run (dir, file) = do
    let src = dir </> file
    res <- parse src
    case res of
      Nothing -> return ()
      Just ast -> do
        ast <- resolve ast
        m <- runMacroCompiler ast
        -- propagating constants and removing useless code
        let r = runRemover $ propagate m
        let curried = curry r
        -- creating a typed AST
        t  <- runInfer curried

        (t', _) <- foldlM (\(acc, i) x -> do
          (acc', i') <- U.runUncurry x i
          return (acc ++ [acc'], i')) ([], M.empty) t

        (c, _) <- foldlM (\(res, c) x -> do
          (output, c') <- runCompiler x c
          return (res ++ [output], c')) ([], M.empty) t'
        let c' = intercalate "\n" $ map from c ++ ["$main(fromList(process.argv));"]
        compile (dir, file) c'



