{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Quark
  import System.FilePath ( (</>), takeDirectory )
  import qualified Data.Map as M
  import Core.Import.Duplicates (getImports)
  import Core.Import.Remover
  import Core.Inference.Type (runInfer, printError)
  import Control.Monad.RWS (liftM, MonadIO (liftIO))
  import Control.Arrow (Arrow(second))
  import Text.Megaparsec (ParseError (FancyError, TrivialError), ParseErrorBundle (ParseErrorBundle), ErrorItem (Tokens, EndOfInput), MonadParsec (parseError))
  import Data.Void (Void)
  import qualified Data.List.NonEmpty as N
  import Data.Maybe (catMaybes)
  import Core.Parser.AST (Expression)
  import Core.Color (bBlack)
  import Core.Closure.Converter (runConverter)
  import Data.Foldable (foldlM)
  
  parseErrors :: ParseErrorBundle String Void -> [(String, Maybe String)]
  parseErrors (ParseErrorBundle msg _) = catMaybes l'
    where l  = N.toList msg
          l' = map (\case
                FancyError _ _ -> Nothing
                TrivialError _ (Just (Tokens _)) _ -> Just ("Unexpected token", Nothing)
                TrivialError _ (Just EndOfInput) _ -> Just ("Unexpected end of line", Just $ bBlack "maybe missing ), ] or }")
                _ -> Nothing) l 

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
                  Left err -> mapM_ (printError . second (Just . show)) err
              Left err -> mapM_ (printError . (,Nothing :: Maybe String)) err
          Left err -> mapM_ printError (parseErrors err)
      Left err -> mapM_ printError (parseErrors err)