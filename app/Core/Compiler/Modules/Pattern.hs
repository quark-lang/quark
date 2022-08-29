module Core.Compiler.Modules.Pattern where
  import Core.Compiler.Definition.IR
  import Core.Inference.Type.AST (Literal(S), TypedPattern(..))
  import Core.Compiler.Definition.Generation
  import qualified Data.Map as M
  import Control.Monad.State (get, zipWithM, MonadIO (liftIO))
  import Data.Maybe (fromJust, isJust)
  import Prelude hiding (and)
  import Debug.Trace (traceShow)

  and :: Expression -> Expression -> Expression
  and l = BinaryCall l "&&"

  -- Returning (Bind, Condition)
  findPattern :: Constructors -> TypedPattern -> Expression -> [(Maybe Expression, Maybe Expression)]
  findPattern env (VarP "true" _) e  = [(Nothing, Just e)]
  findPattern env (VarP "false" _) e = [(Nothing, Just $ Call (Var "!") [e])]
  findPattern env (LitP l _) e       = [(Nothing, Just $ BinaryCall e "==" (Lit l))]
  findPattern env (WilP _) e         = [(Nothing, Nothing)]
  findPattern env (VarP v _) e       = case M.lookup (varify v) env of
    Just _ -> [(Nothing, Just $ BinaryCall (Property e (Var "type")) "==" (Lit $ S v))]
    Nothing -> [(Just $ Let (varify v) e, Nothing)]
  findPattern env (AppP n xs _) e    = 
    concat $ [(Nothing, Just $ BinaryCall (Property e (Var "type")) "==" (Lit (S n)))] : zipWith (\x y -> findPattern env y (Property e (Var x)))
    (["v" ++ show i | i <- [0..]]) xs

  compileCase :: MonadCompiler m => TypedPattern -> m (Expression -> Expression -> Expression)
  compileCase (VarP "true" _) = return $ \x body -> Condition x $ Return body
  compileCase (VarP "false" _) = return $ \x body -> Condition (Call (Var "!") [x]) $ Return body
  compileCase (VarP n _) = do
    get >>= \e -> case M.lookup (varify n) e of
      Just _ -> return $ \x body ->
        let cond = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          in Condition cond $ Return body
      Nothing -> return $ \x body -> Block [Let (varify n) x, Return body]
  compileCase (AppP n args _) = do
    e <- get
    return $ \x b -> do
      let args' = concat $ zipWith (\arg v -> findPattern e arg (Property (Var "$v") (Var v))) args (["v" ++ show i | i <- [0..]])
      let lets   = map (fromJust . fst) $ filter (isJust . fst) args'
      let cs = map (fromJust . snd) $ filter (isJust . snd) args'
      let cond  = BinaryCall (Property (Var "$v") (Var "type")) "===" (Lit (S n))
          conds = case cs of
                    (c:cs) -> cond `and` foldl and c cs
                    _ -> cond
        in Block $ Let "$v" x : [Condition conds $ Block $ lets ++ [Return b]]
  compileCase (WilP _) = do
    return $ \x b -> Return b
  compileCase (LitP l _) = do
    return $ \x b ->
      let cond = BinaryCall x "===" (Lit l)
        in Condition cond $ Return b