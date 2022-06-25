module Core.Import.Remover where
  import Core.Parser.AST
  import Data.Maybe

  removeImport :: Expression -> Maybe Expression
  removeImport (Node (Literal (Identifier "import")) _) = Nothing
  removeImport x = Just x

  runImportRemover :: [Expression] -> [Expression]
  runImportRemover = mapMaybe removeImport