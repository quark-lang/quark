module Core.Macro.Initializing.Remover where
  import Core.Macro.Definition
  import Core.Parser.AST
  import Data.Maybe

  remove :: Expression -> Maybe Expression
  remove (Node (Literal (Identifier "defm")) _) = Nothing
  remove x = Just x

  runRemover :: [Expression] -> [Expression]
  runRemover = mapMaybe remove