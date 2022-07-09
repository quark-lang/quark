module Core.Closure.Definition.Pretty where
  import Core.Closure.Definition.Closure
  import Data.List
  import qualified Data.Map as M
  import Core.Inference.Type.Pretty ()

  instance Show Closure where
    show (Closure name env args body) =
      "Closure " ++ show name ++ ":\n" ++
        "  Env  => " ++ show env ++ "\n" ++
        "  Args => " ++ intercalate ", " (map fst $ M.toList args) ++ "\n" ++
        "  Body => " ++ show body ++ "\n"
