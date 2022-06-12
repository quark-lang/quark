{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Imports where
  import Core.Parser.AST
  import Core.Parser.Utils.Module (parse)
  import Data.Foldable (foldlM)

  resolveImport :: AST -> IO (Maybe [AST])
  resolveImport (Node (Literal "import") [String path]) = do
    x <- parse path
    x' <- case x of
      Just x -> resolve x
      Nothing -> error "Empty module"
    case x' of
      (Node (Literal "begin") [List xs]) -> return $ Just xs
      x -> return $ Just [x]
  resolveImport x = return $ Just [x]


  resolve :: AST -> IO AST
  resolve (Node (Literal "begin") [List xs]) = do
    xs <- foldlM (\acc x -> do
      x <- resolveImport x
      case x of
        Just x -> return (acc ++ x)
        Nothing -> return acc) [] xs
    return $ Node (Literal "begin") [List xs]
