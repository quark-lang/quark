module Core.Macro.Definition.Macro where
  import Data.Map
  import Core.Parser.AST

  data Macro = Macro {
    macroName :: String,
    macroArgs :: [String],
    macroBody :: Expression
  } deriving Show

  type Macros = Map String Macro