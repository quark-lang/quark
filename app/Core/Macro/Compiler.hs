module Core.Macro.Compiler where
  import Core.Macro.Definition
  import Core.Parser.AST
  import Debug.Trace
  import qualified Data.Map as M

  compile :: Expression -> MacroCompiler Expression
  compile z@(Node (Literal (Identifier name)) args) = env >>= \e -> case M.lookup name e of
    Just (Macro _ args1 body) -> do
      if length args1 /= length args
        then throwError $ ["Macro " ++ name ++ " expects " ++ show (length args1) ++ " arguments, but got " ++ show (length args)]
        else do
          let args2 = zip args1 args
          args3 <- mapM (\(x,y) -> compile y >>= \y' -> return (x,y')) args2
          return $ replaceInMacroBody args3 body
    Nothing -> do
      xs <- mapM compile args
      return $ Node (Literal (Identifier name)) xs
  compile (Quoted expr) = return expr
  compile (Node n xs) = do
    xs' <- mapM compile xs
    n'  <- compile n
    return $ Node n' xs'
  compile (Literal l) = return $ Literal l
  compile (List xs) = List <$> mapM compile xs

  replaceInMacroBody :: [(String, Expression)] -> Expression -> Expression
  replaceInMacroBody e (Node n xs) =
    let n' = replaceInMacroBody e n
        xs' = map (replaceInMacroBody e) xs
      in Node n' xs'
  replaceInMacroBody e (Quoted x) = x
  replaceInMacroBody e (Literal (Identifier name)) = case lookup name e of
    Just x -> x
    Nothing -> Literal (Identifier name)
  replaceInMacroBody e (Literal l) = Literal l
  replaceInMacroBody e (List xs) = List $ map (replaceInMacroBody e) xs

  compileMany :: [Expression] -> MacroCompiler [Expression]
  compileMany = mapM compile