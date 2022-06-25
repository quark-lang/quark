{-# LANGUAGE LambdaCase #-}
module Core.Entry where
  import Core.Quark
  import System.FilePath ( (</>) )
  import qualified Data.Map as M
  
  run :: (String, String) -> IO ()
  run (dir, file) = do
    let src = dir </> file
    content <- readFile src
    let ast = parseLisp content
    case ast of
      Right ast -> do
        print ast
        let env = runEnvironments ast
        let x = runMacroCompiler (compileMany $ runRemover ast) env
        print x
      Left err -> print err