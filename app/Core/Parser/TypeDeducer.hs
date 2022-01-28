{-# LANGUAGE LambdaCase, BlockArguments #-}
module Core.Parser.TypeDeducer where
  import qualified Core.Parser.AST as A
  import Core.Parser.Macros
  import Control.Monad.State
  import Data.Maybe
  import Data.List

  -- typechecking transform a basic AST into a typed one
  data Type
    = Lambda [Type] Type
    | Value String
    | None
    deriving (Show, Eq)

  data TypedAST
    = Node TypedAST [TypedAST] Type
    | Integer Integer Type
    | Float Float Type
    | Literal String Type
    deriving (Show, Eq)

  -- some state related useful types
  type Variable = (String, Type)
  type TypingEnvironment = [Variable]
  type Deducer m = (MonadState TypingEnvironment m, MonadIO m)

  getType :: TypedAST -> Type
  getType (Node _ _ t)  = t
  getType (Integer _ t) = t
  getType (Float _ t)   = t
  getType (Literal _ t) = t

  isCall (Node (Literal _ _) _ _) = True
  isCall _ = False

  unpackCall (Node n@(Literal _ _) _ _) = n
  unpackCall _ = error "test"

  changeType :: Deducer m => Variable -> m ()
  changeType v@(n, _) = do
    search n >>= \case
      Nothing -> newType v
      Just x -> do
        env <- get
        let filtered = filter (/=(n, x)) env
          in put $ v : filtered

  removeType :: Deducer m => String -> m ()
  removeType n = modify $ \c -> filter ((/=n) . fst) c

  newType :: Deducer m => Variable -> m ()
  newType v = modify \e -> v : e

  search :: Deducer m => String -> m (Maybe Type)
  search s = get >>= return . lookup s

  isLiteral (Literal _ _) = True
  isLiteral _ = False

  unpackLiteral (Literal n _) = n
  unpackLiteral _ = error "test"

  isLambdaUnfinished (Lambda args ret) = all isLambdaUnfinished args && isLambdaUnfinished ret
  isLambdaUnfinished (Value "a") = True
  isLambdaUnfinished None = False
  isLambdaUnfinished _ = False

  isRetUndefined (Lambda _ (Value "a")) = True
  isRetUndefined _ = False

  replaceRet (Lambda args _) ret = Lambda args ret

  getReturn (Lambda _ ret) = ret
  getReturn x = x

  merge e1 [] = e1
  merge [] e2 = e2
  merge (e1@(n, _):e1s) e2s = if isJust $ find ((==n) . fst) e2s then e1 : merge e1s (filterOnce ((/=n) . fst) e2s) else e1 : merge e1s e2s

  replaceType :: TypedAST -> String -> Type -> TypedAST
  replaceType z@(Literal n _) name t =
    if n == name then Literal n t else z
  replaceType (Node n xs _) name t =
    let n' = (replaceType n name t)
        t' = getReturn $ getType n'
      in Node n' (map (\x -> replaceType x name t) xs) t'
  replaceType x _ _ = x

  infer :: Deducer m => A.AST -> m TypedAST
  infer (A.Node (A.Literal "begin") xs) = do
    env <- get
    t <- mapM infer xs
    put env
    let t' = getType . last $ t
    return $ Node (Literal "begin" t') t t'

  --infer (A.Node (A.Literal "let") [A.Literal name, A.Node (A.Literal "fn") [A.Node (A.Literal "list") args, body]]) = do
  --  mapM (\(A.Literal n) -> changeType (n, Value "a")) args
  --  changeType (name, Lambda (replicate (length args) (Value "a")) (Value "a"))
  --  t <- infer body
  --  search name >>= \case
  --    Nothing -> error $ "Cannot determine at compile time lambda type of " ++ name
  --    Just x@(Lambda args' ret) -> if (not $ isLambdaUnfinished x) || name == "main"
  --      then let x' = if ret == Value "a" then replaceRet x (getReturn (getType t)) else x
  --        in return (Node (Literal "let" None)
  --        [
  --          Literal name x',
  --          Node (Literal "fn" None) [
  --            Node (Literal "list" None)
  --              (map (\(a, t) -> Literal (unliteral a) t) (zip args args')) None,
  --            replaceType t name x'
  --          ] x'
  --        ] None)
  --      else error $ "Lambda type of " ++ name ++ " could not be determined!"

  infer (A.Node (A.Literal "let") [A.Literal name, value]) = do
    case value of
      (A.Node (A.Literal "fn") [A.Node (A.Literal "list") xs, _]) ->
        let t = Lambda (replicate (length xs) (Value "a")) (Value "a")
          in changeType (name, t)
      _ -> return ()
    v <- infer value
    changeType (name, getType v)
    return $ Node (Literal "let" None) [Literal name (getType v), replaceType v name (getType v)] None

  infer (A.Node (A.Literal "fn") [A.Node (A.Literal "list") args, body]) = do
    -- storing environment before processing body
    mapM (\(A.Literal n) -> changeType (n, Value "a")) args
    t <- infer body
    let args' = map unliteral args
    args'' <- map fromJust . filter isJust <$> mapM search args'
    mapM removeType args'
    return $ Node (Literal "fn" None) [
      Node (Literal "list" None) (map (\(n, t) -> Literal n t) (zip args' args'')) None,
      t
      ] (Lambda args'' (getType t))

  infer (A.Node (A.Literal "if") [cond, then', else']) = do
    c <- infer cond
    t <- infer then'
    e <- infer else'
    if getType t == getType e
      then return $ Node (Literal "if" (getType t)) [c, t, e] (getType t)
      else error "Not the same type"

  infer (A.Node n xs) = do
    n'  <- infer n
    xs' <- mapM infer xs
    let (Lambda args ret) = getType n'
    xs'' <-
      mapM (\(x, t) -> if isLiteral x && getType x == Value "a"
        then
          let x' = unpackLiteral x
            in changeType (x', t) >> return (Literal x' t)
        else if isLambdaUnfinished (getType x)
          then let (Node (Literal n _) xs _) = x
                   txs = map getType xs
                  in changeType (n, Lambda txs t) >> return x
          else return x) (zip xs' args)
    return $ Node n' xs'' ret

  infer (A.Literal n) =
    search n >>= return . Literal n . \case
      Nothing -> Value "a"
      Just x -> x

  infer (A.Float f)   = return $ Float f (Value "int")
  infer (A.Integer i) = return $ Integer i (Value "int")

  builtins = [
      ("print", Lambda [Value "int"] (Value "int")),
      ("+", Lambda [Value "int", Value "int"] (Value "int")),
      ("-", Lambda [Value "int", Value "int"] (Value "int")),
      ("*", Lambda [Value "int", Value "int"] (Value "int")),
      ("=", Lambda [Value "int", Value "int"] (Value "int"))
    ]

  runDeducer a = evalStateT (infer a) builtins