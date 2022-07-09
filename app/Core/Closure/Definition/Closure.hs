module Core.Closure.Definition.Closure where
  import Data.Map
  import Core.Inference.Type.AST

  data Closure = Closure {
    name :: String,
    environment :: Map String Type,
    arguments :: Map String Type,
    body :: TypedAST 
  }