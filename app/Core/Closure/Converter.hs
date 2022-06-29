module Core.Closure.Converter where
  import Control.Monad.RWS
  import Core.Closure.Definition
  import Core.Inference.Type.AST
  import qualified Data.Map as M
  import Data.Functor

  type Environment = M.Map String (String, Type)
  type MonadClosure m = (MonadIO m, MonadRWS Environment [Closure] Int m)

  fresh :: MonadClosure m => m String
  fresh = get >>= \n -> put (n + 1) $> "lambda" ++ show n

  addClosure :: MonadClosure m => Closure -> m ()
  addClosure c = tell [c]

  makeClosure :: (String, Type, String) -> Environment -> TypedAST
  makeClosure (n, t, old) env
    = AppE (VarE "make-closure" t) [args] t
    where args = VarE n t

  getSecond :: Ord a => M.Map k (a, b) -> M.Map a b
  getSecond = M.fromList . map snd . M.toList

  replaceNameWith :: TypedAST -> (String, String) -> TypedAST
  replaceNameWith (AppE n a t) z = AppE (replaceNameWith n z) (map (`replaceNameWith` z) a) t
  replaceNameWith (AbsE n a) z = AbsE n (replaceNameWith a z)
  replaceNameWith (LetE n a) z = LetE n (replaceNameWith a z)
  replaceNameWith (VarE n t) z = if fst z == n then VarE (snd z) t else VarE n t
  replaceNameWith (ListE a t) z = ListE (map (`replaceNameWith` z) a) t
  replaceNameWith x _ = x

  createArg :: (a, b) -> (a, (a, b))
  createArg (n, t) = (n, (n, t))

  convert :: MonadClosure m => TypedAST -> m TypedAST
  convert (AbsE n body) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList (map createArg n)
    -- converting also body using the new environment
    b' <- local (`M.union` env) $ convert body
    -- adding the closure to the environment
    e <- ask
    addClosure $ Closure n' (getSecond e) (M.fromList n) b'
    -- replacing lambda with closure maker
    return $ makeClosure (n', map snd n :-> getType b', "") e
  convert (LetE (n, t1) (AbsE args body)) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList (map createArg args)
    -- converting also body using the new environment
    e <- ask
    b' <- local (`M.union` env) $ convert (replaceNameWith body (n, n'))
    -- adding the closure to the environment
    addClosure $ Closure n' (getSecond e) (M.fromList args) b'
    -- building the let expression
    -- building lambda with closure maker
    let c = makeClosure (n', map snd args :-> getType b', n) e
    -- replacing lambda with closure maker
    return $ LetE (n, t1) c
  convert (LetInE (n, t1) (AbsE args body) value) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList (map createArg args)
    -- converting also body using the new environment
    e <- ask
    b' <- local (`M.union` env) $ convert (replaceNameWith body (n, n'))
    -- adding the closure to the environment
    addClosure $ Closure n' (getSecond e) (M.fromList args) b'
    -- building the let expression
    v' <- local (`M.union` M.fromList (map createArg args)) $ convert value
    -- building lambda with closure maker
    let c = makeClosure (n', map snd args :-> getType b', n) e
    -- replacing lambda with closure maker
    return $ LetInE (n, t1) c v'
  convert x = return x
  
  runConverter :: MonadIO m => TypedAST -> Int -> m ([Closure], TypedAST, Int)
  runConverter x i = runRWST (convert x) M.empty i >>= \(t, i', x) -> return (x, t, i')