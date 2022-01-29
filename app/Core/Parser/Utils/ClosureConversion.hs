{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Parser.Utils.ClosureConversion where
  import Core.Parser.Utils.Garbage (removeDuplicates, removeOne)
  import Control.Monad.State
  import Core.Parser.Macros (unliteral, common)
  import Data.List
  import Core.Parser.TypeDeducer
  
  {-
    Module: Closure Conversion
    Description: Defunctionnalizing the lambdas and closures by adding closure environment to the call arguments and putting them at top level.
    Author: thomasvergne
  -}

  {-
    Closure conversion should not put closure environment at beginning of lambda arguments because it would break partial application (currying). So we're building a tuple in which first member is the closure environment and the second represents the lambda itself.
  -}

  data Closure = Closure {
    lambda :: Variable,
    environment :: TypingEnvironment,
    arguments :: TypingEnvironment,
    body :: TypedAST
  } deriving Show

  type Name = String
  type Symbol = (Name, String)
  data ClosureState = ClosureState {
    closures :: [Closure],
    tempEnvironment :: TypingEnvironment,
    lambdaCount :: Int,
    symbols :: [Symbol]
  } deriving Show

  type Converter m = (MonadState ClosureState m, MonadIO m, MonadFail m)

  -- Environment related
  addVariable :: Converter m => Variable -> m ()
  addVariable v = modify $ \c -> c { tempEnvironment = v : tempEnvironment c }

  shiftVariable :: Converter m => m Variable
  shiftVariable =
    gets tempEnvironment >>= \case
      [] -> error "No variable to shift"
      (x:xs) -> do
        modify $ \c -> c { tempEnvironment = xs }
        return x

  changeEnvironment :: Converter m => TypingEnvironment -> m ()
  changeEnvironment e = modify $ \c -> c { tempEnvironment = e }

  -- Closure related
  addClosure :: Converter m => Closure -> m ()
  addClosure f@(Closure (n, _) _ _ _) =
    modify $ \c -> c { closures = f : closures c }

  lambdaPrefix = "lambda"

  createClosureName :: Converter m => m String
  createClosureName = do
    i <- gets lambdaCount
    modify $ \c -> c { lambdaCount = i + 1 }
    return $ lambdaPrefix ++ show i

  nextName :: Converter m => m String
  nextName = (++) <$> pure lambdaPrefix <*> (show <$> (+1) <$> gets lambdaCount)

  replaceClosureName :: TypedAST -> Symbol -> TypedAST
  replaceClosureName z@(Literal n t) (name, replace) =
    if n == name then Literal replace t else z
  replaceClosureName (Node n xs t) i =
    Node (replaceClosureName n i) (map (`replaceClosureName` i) xs) t
  replaceClosureName x _ = x

  searchClosure :: Converter m => Variable -> m (Maybe Closure)
  searchClosure v = gets closures >>= return . find ((==v) . lambda)

  -- Symbol related
  addSymbol :: Converter m => Symbol -> m ()
  addSymbol s = modify $ \c -> c { symbols = s : symbols c }

  removeSymbol :: Converter m => Name -> m ()
  removeSymbol x = modify $ \c -> c { symbols = filterOnce ((/=x) . fst) (symbols c) }

  searchSymbol :: Converter m => Name -> m (Maybe String)
  searchSymbol s = gets symbols >>= return . lookup s

  -- Useful functions
  filterOnce :: (a -> Bool) -> [a] -> [a]
  filterOnce f xs = f' f xs 0
    where f' :: (a -> Bool) -> [a] -> Int -> [a]
          f' _ [] _ = []
          f' f (x:xs) i = if f x && i == 0
            then x : f' f xs (i + 1)
            else f' f xs i

  typedUnliteral (Literal n t) = (n, t)
  typedUnliteral _ = error "test"

  -- Closure conversion
  convert :: Converter m => TypedAST -> m TypedAST
  convert (Node (Literal "let" _) [Literal name t, Node (Literal "fn" _) [Node (Literal "list" _) args _, body] _] t1) = do
    next <- case name of
      "main" -> return "main"
      _ -> createClosureName
    env <- gets tempEnvironment

    let args' = map typedUnliteral args
    mapM addVariable args'

    addSymbol (name, next)
    body' <- convert body

    current <- common env <$> gets tempEnvironment
    changeEnvironment env

    let closure = Closure (next, t) current args' body'
    addClosure closure
    return $ Node (Literal "make-closure" None) (Literal next t : map (\(n, t) -> Literal n t) current) None

  convert (Node (Literal "fn" _) [Node (Literal "list" _) args _, body] t) = do
    env <- gets tempEnvironment
    next <- createClosureName
    let args' = map typedUnliteral args
    mapM addVariable args'

    body' <- convert body

    current <- common env <$> gets tempEnvironment
    changeEnvironment env
    let closure = Closure (next, t)  current args' body'
    addClosure closure
    return $ Node (Literal "make-closure" None) (Literal next t : map (\(n, t) -> Literal n t) current) None

  convert (Node (Literal "let" _) [Literal name t, value] t1) = do
    addVariable (name, t)
    value' <- convert value
    return $ Node (Literal "let" None) [Literal name t, value'] t1

  convert (Node (Literal "begin" t1) xs t2) = do
    env <- gets tempEnvironment
    xs' <- mapM convert xs
    changeEnvironment env
    return $ Node (Literal "begin" t1) xs' t2

  convert (Node (Literal "drop" t) [x] t') = do
    Literal name t1 <- convert x
    removeSymbol name
    return $ Node (Literal "drop" t) [Literal name t1] t'

  convert (Node n xs t) = Node <$> convert n <*> mapM convert xs <*> pure t
  convert (Literal name t) = Literal <$> (searchSymbol name >>= return . \case
    Nothing -> name
    Just x -> x) <*> pure t
  convert x = return x

  runConverter x = execStateT (convert x) (ClosureState [] [] 0 [])