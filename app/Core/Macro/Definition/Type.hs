module Core.Macro.Definition.Type where
  import Core.Macro.Definition.Macro
  import Control.Monad.Except
  import Control.Monad.State
  import qualified Data.Map as M

  newtype MacroCompiler a = MacroCompiler
    { runMacroCompiler :: Macros -> Either [String] a }
    
  instance Functor MacroCompiler where
    fmap f (MacroCompiler m) = MacroCompiler $ fmap (fmap f) m

  instance Applicative MacroCompiler where
    pure x = MacroCompiler $ \_ -> pure x
    (MacroCompiler f) <*> (MacroCompiler x)
      = MacroCompiler $ \s -> f s <*> x s

  instance Monad MacroCompiler where
    return = pure
    (MacroCompiler m) >>= f
      = MacroCompiler $ \s -> do
          x <- m s
          runMacroCompiler (f x) s

  throwError :: [String] -> MacroCompiler a
  throwError = MacroCompiler . const . Left

  env :: MacroCompiler Macros
  env = MacroCompiler $ \s -> Right s