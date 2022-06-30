{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Modules.Pattern where
  import Core.Compiler.Definition.IR
  import Core.Inference.Type.AST (Literal(S), TypedPattern(..))
  import Core.Compiler.Definition.Generation
  import qualified Data.Map as M
  import Control.Monad.State (get)
  import Data.Maybe (fromJust, isJust)
  import Prelude hiding (and)
  
  and :: Expression -> Expression -> Expression
  and l = BinaryCall l "&&"

  compileCase :: MonadCompiler m => TypedPattern -> m (Expression -> Expression -> Expression)
  compileCase (VarP n _) = do
    get >>= \e -> case M.lookup (varify n) e of
      Just _ -> return $ \x body ->
        let cond = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          in Condition cond $ Return body
      Nothing -> return $ \x body -> Block [Let n x, Return body]
  compileCase (AppP n args _) = do
    let args' = zipWith (curry (\case
                  (VarP n _, i) -> (Just $ \x -> Let (varify n) $ Property x (Var i), Nothing)
                  (LitP l _, i) -> (Nothing, Just $ \x ->
                    BinaryCall (Property x (Var i)) "===" (Lit l))
                  (WilP _, i) -> (Nothing, Nothing)
                  _ -> error "Pattern must be one level")) args (["v" ++ show i | i <- [0..]])
    let lets   = map (fromJust . fst) $ filter (isJust . fst) args'
    let cs = map (fromJust . snd) $ filter (isJust . snd) args'
    return $ \x b ->
      let cond  = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          conds = case cs of
                    (c:cs) -> cond `and` foldl (\acc f -> acc `and` f x) (c x) cs
                    _ -> cond
        in Condition conds $ Block $ map (\f -> f x) lets ++ [Return b]
  compileCase (WilP _) = do
    return $ \x b -> Return b
  compileCase (LitP l _) = do
    return $ \x b ->
      let cond = BinaryCall x "===" (Lit l)
        in Condition cond $ Return b