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

  run :: (String, String) -> IO ()
  run (dir, file) = do
    let src = dir </> file
    content <- readFile src
    let ast = parseLisp content
    case ast of
      Right ast -> do
        ast' <- fmap (map snd) <$> getImports (takeDirectory src) ast
        let res = fmap concat ((++[runImportRemover ast]) <$> ast')
        case res of
          Right ast' -> do
            let env = runEnvironments ast'
            let x = runMacroCompiler (compileMany $ runMacroRemover ast') env
            case x of
              Right x -> do
                x' <- runInfer x
                case x' of
                  Right x ->do
                    (x, closures, _) <- foldlM (\(acc, cl, i) x -> do
                      (cl', x', i') <- runConverter x i
                      return (acc ++ [x'], cl ++ cl', i')) ([], [], 0) x
                    mapM_ print x
                    mapM_ print closures
                  Left err -> (printError . second (Just . show)) err
              Left err -> (printError . (,Nothing)) err
          Left err -> printError (parseError err)
      Left err -> printError (parseError err)