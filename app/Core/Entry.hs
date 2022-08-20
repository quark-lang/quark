{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Quark
  import System.FilePath ( (</>), takeDirectory )
  import qualified Data.Map as M
  import Core.Import.Duplicates (getImports)
  import Core.Import.Remover
  import Core.Inference.Type (runInfer)
  import Control.Monad.RWS (liftM, MonadIO (liftIO))
  import Control.Arrow (Arrow(second))
  import Core.Closure.Converter (runConverter)
  import Data.Foldable (foldlM)
  import Core.Utility.Error (parseError, printError)
  import Core.Constant.Propagation (propagate)
  import Core.Compiler.Compiler (runCompiler)
  import Core.Compiler.Definition.Generation (from)
  import Core.Utility.Sugar (eliminateSugar)
  import System.Environment (getEnv)
  import Core.Utility.InstanceResolver (addArgument)
  
  run :: (String, String) -> IO (Either (String, Maybe String) String)
  run (dir, file) = do
    let src = dir </> file
    content <- readFile src
    let ast = parseLisp content
    case ast of
      Right ast -> do
        ast' <- runImport (takeDirectory src) ast
        let res = runImportRemover <$> ast'
        case res of
          Right ast' -> do
            let env = runEnvironments ast'
            let x = map eliminateSugar <$> runMacroCompiler (compileMany $ runMacroRemover ast') env
            case fmap (map propagate) x of
              Right x -> do
                x' <- runInfer x
                case x' of
                  Right (x, insts) -> do
                    mapM_ (print . fst . addArgument False insts) x
                    --(x, closures, _) <- foldlM (\(acc, cl, i) x -> do
                    --  (cl', x', i') <- runConverter x i
                    --  return (acc ++ [x'], cl ++ cl', i')) ([], [], 0) x
                    (c, _) <- foldlM (\(acc, st) x -> do
                      (x', st') <- runCompiler x st
                      return (acc ++ [x'], st')) ([], M.empty) $ map (fst . addArgument False insts) x
                    path <- getEnv "QUARK"
                    placeholder <- readFile (path </> "app/Core/placeholder.js")
                    return . Right $ placeholder ++ concatMap ((++";") . from) c ++ "$main();"
                  Left err -> return $ Left (second (Just . show) err)
              Left err -> return $ Left (err, Nothing)
          Left err -> return $ Left (parseError err)
      Left err -> return $ Left (parseError err)