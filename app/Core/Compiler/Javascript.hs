{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Javascript where
  import Prelude hiding (and)
  import Data.List hiding (and)
  import Data.Char
  import Control.Monad.State
  --import Core.Parser.Utils.ClosureConversion hiding (lambdaCount, environment, Name)
  import Core.Compiler.Uncurry hiding (getReturn)
  import Data.Maybe
  import qualified Data.Map as M
  --import Core.Inference.Type.Pretty
  import Control.Arrow (Arrow(second))
  import Data.Traversable (for)
  import qualified Core.Inference.Type.AST as A
  import Data.Foldable hiding (and)
  import Core.Inference.Type.AST hiding (TypedAST(..), TypedPattern(..), Type(..))
  import Text.Printf (printf)

  {-
    Module: CLang compilation
    Description: Output C from Quark AST
    Author: thomasvergne
  -}

  type Constructors = M.Map String String
  type MonadCompiler m = (MonadState Constructors m, Monad m, MonadIO m)

  -- Code representation


  encodeUnicode16 :: String -> String
  encodeUnicode16 = concatMap escapeChar
    where
      escapeChar c
          | ' ' <= c && c <= 'z' = [c]
          | otherwise =
              printf "\\u%04x" (fromEnum c)


  data Expression
    -- Expressions
    = Lambda [String] Expression
    | Var String
    | Lit Literal
    | Object [(String, Expression)]
    | Ternary Expression Expression Expression
    | Array [Expression]
    | Call Expression [Expression]
    | Property Expression Expression
    | BinaryCall Expression String Expression

    -- Statements
    | Let String Expression
    | Function String [String] Expression
    | Condition Expression Expression
    | Return Expression
    | Block [Expression]
    | Throw Expression
    | Require String
    deriving (Show, Eq)

  from :: Expression -> String
  from (Lambda args expr@(Object _)) = "(" ++ intercalate "," args ++ ") => (" ++ from expr ++ ")"
  from (Lambda args expr) = "(" ++ intercalate "," args ++ ") => " ++ from expr
  from (Var n) = n
  from (Object props) = "{" ++ intercalate "," (map (\(p, v) -> p  ++ ": " ++ from v) props) ++ "}"
  from (Ternary t c e) = from t ++ " ? " ++ from c ++ " : " ++ from e
  from (Array exprs) = "[" ++ intercalate "," (map from exprs) ++ "]"
  from (Call (Var n) xs) = n ++ "(" ++ intercalate "," (map from xs) ++ ")"
  from (Call z@(Property _ _) xs) = from z ++ "(" ++ intercalate "," (map from xs) ++ ")"
  from (Call n xs) = "(" ++ from n ++ ")(" ++ intercalate "," (map from xs) ++ ")"
  from (Property n z@(Lit _)) = from n ++ "[" ++ from z ++ "]"
  from (Property n p) = from n ++ "." ++ from p
  from (BinaryCall l op r) = from l ++ " " ++ op ++ " " ++ from r
  from (Let n v) = "var " ++ n ++ " = " ++ from v
  from (Condition c t) = "if (" ++ from c ++ ") " ++ from t
  from (Return e) = "return " ++ from e
  from (Block exprs) = "{" ++ concatMap ((++";") . from) exprs ++ "}"
  from (Throw e) = "throw " ++ from e
  from (Lit (S s)) = "\"" ++ encodeUnicode16 s ++ "\""
  from (Lit (F f)) = show f
  from (Lit (I i)) = show i
  from (Require p) = "Object.entries(require(\"" ++ p ++ "\")).map(([name, exported]) => global[name] = exported);"
  from _ = error "from: not implemented"

  addCons :: MonadCompiler m => (String, String) -> m ()
  addCons (k, a) = modify $ \c -> M.insert k a c

  compileData :: MonadCompiler m => UncurriedAST -> m Expression
  compileData (DataE (n, _) cons) = do
    cons' <- mapM (\(n', t) -> do
              addCons (n', n)
              case t of
                args :-> _ -> do
                  let args' = zipWith const (["v" ++ show i | i <- [0..]]) args
                  let fields = map (\x -> (x, Var x)) args'
                  return (n', Lambda args' (Object (("type", Lit (S n')) : fields)))
                _ -> return (n', Object [("type", Lit (S n'))])) cons
    return $ Let n (Object cons')
  compileData _ = error "compileData: not a data"

  type Variables    = [String]
  type Conditions   = [String]
  type AccessValues = [String]

  and :: Expression -> Expression -> Expression
  and l = BinaryCall l "&&"

  compileCase :: MonadCompiler m => UncurriedPattern -> m (Expression -> Expression -> Expression)
  compileCase (VarP n _) = do
    get >>= \e -> case M.lookup n e of
      Just _ -> return $ \x body ->
        let cond = BinaryCall (Property x (Var "type")) "===" (Lit (S n))
          in Condition cond $ Return body
      Nothing -> return $ \x body -> Block [Let n x, Return body]
  compileCase (AppP (n, _) args _) = do
    let args' = zipWith (curry (\case
                  (VarP n _, i) -> (Just $ \x -> Let n $ Property x (Var i), Nothing)
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

  compilePattern :: MonadCompiler m => UncurriedAST -> m Expression
  compilePattern (PatternE x pats) = do
    x <- compile x
    Block <$> mapM (\(p, b) -> do
      b <- compile b
      p <- compileCase p
      return $ p x b) pats
  compilePattern _ = error "compilePattern: not a pattern"

  compile :: MonadCompiler m => UncurriedAST -> m Expression
  -- JS AST Generation
  compile (AppE ("Call", _) [n] _) = Call <$> compile n <*> pure []
  compile (AppE ("Call", _) [n, x] _) = Call <$> compile n <*> ((:[]) <$> compile x)
  compile (AppE ("Property", _) [obj, prop] _) = Property <$> compile obj <*> compile prop
  compile (AppE ("Var", _) [LitE (S x) _] _) = return $ Var x
  compile (AppE ("Throw", _) [x] _) = Throw <$> compile x
  compile (AppE ("Block", _) xs _) = Block <$> mapM compile xs
  compile (AppE ("require", _) [LitE (S path) _] _) = return $ Require path

  -- Binary calls
  compile (AppE ("*", _) [x, y] _) = BinaryCall <$> compile x <*> pure "*" <*> compile y
  compile (AppE ("=", _) [x, y] _) = BinaryCall <$> compile x <*> pure "===" <*> compile y
  compile (AppE ("-", _) [x, y] _) = BinaryCall <$> compile x <*> pure "-" <*> compile y
  compile (AppE ("+", _) [x, y] _) = BinaryCall <$> compile x <*> pure "+" <*> compile y
  compile (AppE ("/", _) [x, y] _) = BinaryCall <$> compile x <*> pure "/" <*> compile y

  compile (AppE (n, _) args _) = get >>= \e -> case M.lookup n e of
    -- Checking if it's a constructor
    Just obj -> Call (Property (Var obj) (Var n)) <$> mapM compile args
    Nothing -> Call (Var n) <$> mapM compile args
  compile (AbsE args body) = Lambda (map fst args) <$> compile body
  compile (VarE t _) = get >>= \e -> case M.lookup t e of
    Just obj -> return $ Property (Var obj) (Var t)
    Nothing -> return $ Var t
  compile (LetInE (n, _) value expr) = do
    value <- compile value
    expr  <- compile expr
    return $ Call (Lambda [n] expr) [value]
  compile (ListE exprs _) = Array <$> mapM compile exprs
  compile (LetE (n, _) value) = Let n <$> compile value
  compile (LitE l _) = return $ Lit l
  compile (IfE c t e) = do
    c <- compile c
    t <- compile t
    e <- compile e
    return $ Ternary c t e
  compile z@(DataE _ _) = compileData z
  compile z@(PatternE _ _) = compilePattern z

  runCompiler :: (Monad m, MonadIO m) => UncurriedAST -> Constructors -> m (Expression, Constructors)
  runCompiler a = runStateT (compile a)
