module Core.Import.Type where
  import Core.Parser.AST (Expression)
  type Path = String
  type ImportMap = [(Path, [Expression])]

  appendPath :: (Path, [Expression]) -> ImportMap -> ImportMap
  appendPath z@(path, _) map = map' ++ [z]
    where map' = filter ((/=path) . fst) map

  removeDuplicates :: (Eq a) => [a] -> [a]
  removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
  removeDuplicates [] = []

  mergePaths :: ImportMap -> ImportMap -> ImportMap
  mergePaths xs ys = removeDuplicates . reverse $ foldr (\x -> (++) (appendPath x ys)) ys xs