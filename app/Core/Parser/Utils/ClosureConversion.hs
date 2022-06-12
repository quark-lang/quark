{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Parser.Utils.ClosureConversion where
  import Core.Parser.Utils.Garbage (removeDuplicates, removeOne)
  import Control.Monad.State
  import Core.Parser.Macros (unliteral, common)
  import Data.List
  import qualified Data.Map as M
  import Data.Functor
  import Control.Monad.RWS
  import Debug.Trace
  import Core.Inference.Type.AST
  import Core.Inference.Type.Methods
  
  {-
    Module: Closure Conversion
    Description: Defunctionnalizing the lambdas and closures by adding closure environment to the call arguments and putting them at top level.
    Author: thomasvergne
  -}

  {-
    Closure conversion should not put closure environment at beginning of lambda arguments because it would break partial application (currying). So we're building a tuple in which first member is the closure environment and the second represents the lambda itself.
  -}

  type Environment = M.Map String (String, Type)

  data ClosureAST
    = Close Closure
    | Leaf TypedAST
    deriving Show

  data Closure = Closure {
    name :: String,
    environment :: M.Map String Type,
    argument :: (String, Type),
    body :: TypedAST
  }
  instance Show Closure where
    show (Closure name env arg body) 
      = "Closure:\n" ++ 
          "  name => " ++ name ++ "\n" ++ 
          "  env  => " ++ show (M.toList env) ++ "\n" ++ 
          "  arg => " ++ show arg ++ "\n" ++ 
          "  body => " ++ show body

  type Converter a = (MonadRWS Environment [Closure] Int a, MonadIO a)

  fresh :: Converter m => m String
  fresh = get
    >>= \n -> put (n + 1) >> return ("lambda" ++ show n)

  addClosure :: Converter m => Closure -> m ()
  addClosure c = tell [c]

  makeClosure :: (String, Type, String) -> Environment -> TypedAST
  makeClosure (n, t, old) env
    = AppE (VarE "make-closure" t) args t
    where args = ListE [VarE n t] t

  getSecond :: Ord a => M.Map k (a, b) ->M.Map a b
  getSecond = M.fromList . map snd . M.toList

  replaceNameWith :: TypedAST -> (String, String) -> TypedAST
  replaceNameWith (AppE n a t) z = AppE (replaceNameWith n z) (replaceNameWith a z) t
  replaceNameWith (AbsE n a) z = AbsE n (replaceNameWith a z)
  replaceNameWith (LetE n a) z = LetE n (replaceNameWith a z)
  replaceNameWith (VarE n t) z = if fst z == n then VarE (snd z) t else VarE n t
  replaceNameWith (ListE a t) z = ListE (map (`replaceNameWith` z) a) t
  replaceNameWith x _ = x

  getType :: TypedAST -> Type
  getType (AppE _ _ t) = t
  getType (AbsE (_, t) b) = t :-> getType b
  getType (LetE (n, a) _) = a
  getType (VarE _ t) = t
  getType (ListE a t) = t
  getType x = error $ "getType: " ++ show x

  convert :: Converter m => TypedAST -> m TypedAST
  convert (AbsE (n, t) body) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList [(n, (n, t))]
    -- converting also body using the new environment
    b' <- local (`M.union` env) $ convert body
    -- adding the closure to the environment
    e <- ask
    addClosure $ Closure n' (getSecond e) (n, t) b'
    -- replacing lambda with closure maker
    return $ makeClosure (n', t :-> getType b', "") e
  convert (LetE (n, t1) (AbsE (a, t2) body)) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList [(a, (a, t2))]
    -- converting also body using the new environment
    e <- ask
    b' <- local (`M.union` env) $ convert (replaceNameWith body (n, n'))
    -- adding the closure to the environment
    addClosure $ Closure n' (getSecond e) (a, t2) b'
    -- building the let expression
    -- building lambda with closure maker
    let c = makeClosure (n', t2 :-> getType b', n) e
    -- replacing lambda with closure maker
    return $ LetE (n, t1) c
  convert (LetInE (n, t1) (AbsE (a, t2) body) value) = do
    -- creating a new lambda name
    n' <- fresh
    -- building the environment based on self and argument
    let env = M.fromList [(a, (a, t2))]
    -- converting also body using the new environment
    e <- ask
    b' <- local (`M.union` env) $ convert (replaceNameWith body (n, n'))
    -- adding the closure to the environment
    addClosure $ Closure n' (getSecond e) (a, t2) b'
    -- building the let expression
    v' <- local (`M.union` M.fromList [(a, (a, t2))]) $ convert value
    -- building lambda with closure maker
    let c = makeClosure (n', t2 :-> getType b', n) e
    -- replacing lambda with closure maker
    return $ LetInE (n, t1) c v'
  convert x = return x

  convertClosures :: (Monad m, MonadIO m) => TypedAST -> Int -> m ([Closure], TypedAST, Int)
  convertClosures a i
    = runRWST (convert a) M.empty i
        >>= \(t, i', x) -> return (x, t, i')