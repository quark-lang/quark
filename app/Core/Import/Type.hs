module Core.Import.Type where
  import Core.Parser.AST (Expression)
  import Debug.Trace (traceShow)
  import Data.List (union)
  type Path = String
  type ImportMap = [(Path, [Expression])]

  prependPath :: (Path, [Expression]) -> ImportMap -> ImportMap
  prependPath z@(path, _) map = z : map'
    where map' = filter ((/=path) . fst) map

  removeDuplicates :: (Eq a) => [a] -> [a]
  removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
  removeDuplicates [] = []

  mergePaths :: ImportMap -> ImportMap -> ImportMap
  mergePaths [] ys = ys
  mergePaths (x:xs) ys = x : (xs `union` ys)