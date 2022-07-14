module Core.Macro.Initializing.Environment where
  import Core.Macro.Definition
  import Core.Parser.AST
  import qualified Data.Map as M

  isID :: Expression -> Bool
  isID (Identifier _) = True
  isID _ = False

  identifiers :: [Expression] -> [String]
  identifiers e = map (\(Identifier s) -> s) e'
    where e' = filter isID e

  runMacroEnvironment :: Expression -> Macros
  runMacroEnvironment (Node (Identifier "defm") [Identifier name, List args, body]) =
    let macro = Macro name (identifiers args) body
      in M.singleton name macro
  runMacroEnvironment (Node (Identifier "defm") [Identifier name, value]) =
    let macro = Macro name [] value
      in M.singleton name macro
  runMacroEnvironment _ = M.empty

  runEnvironments :: [Expression] -> Macros
  runEnvironments = M.unions . map runMacroEnvironment