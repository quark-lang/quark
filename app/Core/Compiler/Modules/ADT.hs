module Core.Compiler.Modules.ADT where
  import Core.Compiler.Definition.IR
  import Core.Inference.Type.AST
  import qualified Data.Map as M
  import Control.Monad.State
  import Core.Compiler.Definition.Generation

  addCons :: MonadCompiler m => (String, String) -> m ()
  addCons (k, a) = modify $ \c -> M.insert k a c

  compileData :: MonadCompiler m => TypedAST -> m Expression
  compileData (DataE (n, _) cons) = do
    cons' <- mapM (\(n', t) -> do
              addCons (varify n', varify n)
              case t of
                args :-> _ -> do
                  let args' = zipWith const (["v" ++ show i | i <- [0..]]) args
                  let fields = map (\x -> (x, Var x)) args'
                  return (varify n', Lambda args' (Object (("type", Lit (S n')) : fields)))
                _ -> return (varify n', Object [("type", Lit (S n'))])) cons
    return $ Let (varify n) (Object cons')
  compileData _ = error "compileData: not a data"