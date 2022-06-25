module Core.Macro.Initializing.Remover where
  import Core.Parser.AST
  import Data.Maybe

  remove :: Expression -> Maybe Expression
  remove (Node (Literal (Identifier "defm")) _) = Nothing
  remove x = Just x

  runMacroRemover :: [Expression] -> [Expression]
  runMacroRemover = mapMaybe remove