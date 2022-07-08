module Core.Compiler.Modules.Pattern where
  import Core.Compiler.Definition.IR
  import Core.Inference.Type.AST (Literal(S), TypedPattern(..))
  import Core.Compiler.Definition.Generation
  import qualified Data.Map as M
  import Control.Monad.State (get)
  import Data.Maybe (fromJust, isJust)
  import Prelude hiding (and)
  import Debug.Trace (traceShow)

  and :: Expression -> Expression -> Expression
  and l = BinaryCall l "&&"

  -- Returning (Bind, Condition)
  findPattern :: TypedPattern -> Expression -> [(Maybe Expression, Maybe Expression)]
  findPattern (VarP "true" _) e  = [(Nothing, Just e)]
  findPattern (VarP "false" _) e = [(Nothing, Just $ Call (Var "!") [e])]
  findPattern (LitP l _) e       = [(Nothing, Just $ BinaryCall e "==" (Lit l))]
  findPattern (WilP _) e         = [(Nothing, Nothing)]
  findPattern (VarP v _) e       = [(Just $ Let (varify v) e, Nothing)]
  findPattern (AppP n xs _) e    = 
    concat $ [(Nothing, Just $ BinaryCall (Property e (Var "type")) "==" (Lit (S n)))] : zipWith (\x y -> findPattern y (Property e (Var x)))
    (["v" ++ show i | i <- [0..]]) xs

  compileCase :: MonadCompiler m => TypedPattern -> m (Expression -> Expression -> Expression)
  compileCase (VarP "true" _) = return $ \x body -> Condition x $ Return body
  compileCase (VarP "false" _) = return $ \x body -> Condition (Call (Var "!") [x]) $ Return body
  compileCase (VarP n _) = do
    get >>= \e -> case M.lookup (varify n) e of
      Just _ -> return $ \x body ->
        let cond = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          in Condition cond $ Return body
      Nothing -> return $ \x body -> Block [Let n x, Return body]
  compileCase (AppP n args _) = do
    return $ \x b -> do
      let args' = concat $ zipWith (\arg v -> findPattern arg (Property x (Var v))) args (["v" ++ show i | i <- [0..]])
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args'
      let cs = map (fromJust . snd) $ filter (isJust . snd) args'
      let cond  = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          conds = case cs of
                    (c:cs) -> cond `and` foldl and c cs
                    _ -> cond
        in Condition conds $ Block $ lets ++ [Return b]
  compileCase (WilP _) = do
    return $ \x b -> Return b
  compileCase (LitP l _) = do
    return $ \x b ->
      let cond = BinaryCall x "===" (Lit l)
        in Condition cond $ Return b