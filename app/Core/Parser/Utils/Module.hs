{-# LANGUAGE LambdaCase #-}
module Core.Parser.Utils.Module where
  import qualified Core.Parser.Parser as Parser
  import Core.Parser.AST        (AST(..))
  import System.Directory       (doesFileExist)
  import System.FilePath        ((</>), dropFileName)
  import Core.Parser.Combinator (runParser, many)
  import Data.Functor           ((<&>))
  import Data.Foldable          (foldlM)
  import Data.Char              (ord)

  {-
    Module: Parser utils
    Description: Set of functions to extra-parse AST 
    Author: thomasvergne
  -}

  parse :: String -> IO (Maybe AST)
  parse file = do
    exists <- doesFileExist file
    if exists
      then do
        content <- Parser.format <$> readFile file
        let (res, _) = runParser (many Parser.parse) content
        case res of
          Left err -> print err >> return Nothing
          Right ast ->
            let ast' = if length ast > 1 then Node (Literal "begin") [List ast] else head ast
              in Just <$> visitAST (dropFileName file) ast'
      else print ("File " ++ file ++ " does not exist") >> return Nothing

  -- take a source path and may return AST module
  resolveImport :: (String, String) -> IO (Maybe AST)
  resolveImport (base, path) = parse $ base </> path

  visitAST :: String -> AST -> IO AST
  -- resolving import
  visitAST p z@(Node (Literal "import") [String path]) =
    resolveImport (p, path) >>= \case
      -- if importing does not work, return the original node
      Nothing -> return z
      -- return a node which gonna be spread
      Just ast -> return $ Node (Literal "spread") [ast]

  -- Converting list to their Cons/Nil representation
  visitAST p (List xs) = visitAST p . buildList =<< mapM (visitAST p) xs
  visitAST p (Node (Literal "fn") (List args:body:_)) = buildClosure args <$> visitAST p body

  visitAST _ z@(Node (Literal "declare") [n, c]) = return z
  visitAST _ z@(Node (Literal "data") [n, c]) = return z
  visitAST p z@(Node (Literal "data") [n, c, expr]) = do
    expr' <- visitAST p expr
    return $ Node (Literal "data") [n, c, expr']
  visitAST p z@(Node (Literal "declare") [n, c, expr]) = do
    expr' <- visitAST p expr
    return $ Node (Literal "declare") [n, c, expr']

  visitAST p z@(Node (Literal "begin") [List xs]) = do
    xs' <- mapM (visitAST p) xs
    return $ Node (Literal "begin") [List xs']

  visitAST p a@(Node n z) = do
    -- building new children by folding
    xy <- foldlM (\a x -> do
      x <- visitAST p x
      case x of
        -- spreading function just put 
        -- children content in the current built
        Node (Literal "spread") [Node (Literal "begin") xs] -> return $ a ++ xs
        _ -> return $ a ++ [x]) [] z
    r <- visitAST p n
    let r' = case r of
          Node (Literal "spread") [xs] -> xs
          _ -> r
    return $ if r' `elem` reserved then Node r' xy else buildCall r' xy

  -- Value visiting
  visitAST _ i@(Integer _) = return i
  -- Converting string to list of chars
  visitAST p s@(String _)  = return s
  visitAST _ f@(Float _)   = return f
  visitAST _ s@(Literal _) = return s
  -- Converting char to integer
  visitAST _   (Char c)    = return . Node (Literal "chr") $ [Integer . toInteger . ord $ c]

  reserved = map Literal ["begin", "fn", "spread", "chr", "let", "declare", "->", "match", "data"]

  convertString :: String -> AST
  convertString s = Node (Literal "list") $ map Char s

  buildClosure :: [AST] -> AST -> AST
  buildClosure [] b = b
  buildClosure (x:xs) b = Node (Literal "fn") [x, buildClosure xs b]

  buildList :: [AST] -> AST
  buildList [] = Node (Literal "Nil") []
  buildList (x:xs) = Node (Literal "Cons") [x, buildList xs]

  buildCall :: AST -> [AST] -> AST
  buildCall f [] = f
  buildCall call (x:args) = buildCall (Node call [x]) args