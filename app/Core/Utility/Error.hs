{-# LANGUAGE LambdaCase #-}
module Core.Utility.Error where
  import Core.Utility.Color (bBlack, red)
  import Text.Megaparsec.Error
  import qualified Data.List.NonEmpty as N
  import Data.Void (Void)
  import Data.Maybe
  import Control.Monad.Cont (MonadIO (liftIO))
    
  parseError :: ParseErrorBundle String Void -> (String, Maybe String)
  parseError (ParseErrorBundle msg _) = head $ catMaybes l'
    where l  = N.toList msg
          l' = map (\case
                FancyError  _ _ -> Nothing
                TrivialError _ (Just (Tokens _)) _ -> Just ("Unexpected token", Nothing)
                TrivialError _ (Just EndOfInput) _ -> Just ("Unexpected end of line", Just $ bBlack "maybe missing ), ] or }")
                _ -> Nothing) l 

  printError :: MonadIO m => (String, Maybe String) -> m ()
  printError (error, ast) = liftIO $ do
    putStr (red "[error] ")
    putStrLn error
    case ast of
      Just ast -> putStr "  with " >> putStrLn ast
      Nothing -> return ()