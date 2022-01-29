{-# LANGUAGE TupleSections, LambdaCase #-}
module Core.Compiler.CLang where
  import Data.List
  import Data.Char
  import Control.Monad.State
  import Core.Parser.TypeDeducer hiding (isChar)
  import Core.Parser.Utils.ClosureConversion hiding (lambdaCount, environment, Name)
  {-
    Module: CLang compilation
    Description: Output C from Quark AST
    Author: thomasvergne
  -}

  type LambdaType = String
  type EnvVariable = (String, String, String) -- (name, type, closure)

  data C = C {
    lambdaTypes :: [LambdaType],
    functions :: [CAST],
    temporary :: [String],
    lambdaCount :: Int,
    environment :: [EnvVariable]
  } deriving Show

  libraries = ["stdio.h"]

  loadLibrary n = "#include <" ++ n ++ ">"

  outputC :: C -> String
  outputC (C types functions _ _ _) =
    let funcs' = reverse $ map fromAST functions
        types' = map (++";") $ reverse types
        libs   = map loadLibrary libraries
      in concat $ intersperse "\n" (libs ++ [""] ++ types' ++ funcs')

  type Argument = CAST
  type Name = CAST
  data CAST
    = Block [CAST]
    | Function Name [Argument] CAST
    | Declaration Name CAST
    | Assignment Name CAST
    | BinaryOperation String CAST CAST
    | FunctionCall Name [Argument] String
    | Condition CAST CAST CAST
    | Return CAST

    | Identifier String String
    | Int Int
    | Flt Float
    | Array [CAST] String
    | Char Int

    | Spread [CAST]
    deriving Show

  fromAST :: CAST -> String
  fromAST (Block xs) = "{" ++ (concat $ map ((++";") . fromAST) xs) ++ "}"
  fromAST (Function (Identifier name ret) args body) =
    ret ++ " " ++ name ++ "(" ++ (concat $ intersperse "," (map fromAST args)) ++ ")" ++ fromAST body
  fromAST (Declaration (Identifier name t) value) =
    t ++ " " ++ name ++ " = " ++ fromAST value
  fromAST (Assignment (Identifier name _) value) =
    name ++ " = " ++ fromAST value
  fromAST (BinaryOperation op x y) = fromAST x ++ " " ++ op ++ " " ++ fromAST y
  fromAST (FunctionCall fun args t) =
    "((" ++ t ++ ")" ++ " " ++ fromAST fun ++ "(" ++ (concat $ intersperse "," (map fromAST args)) ++ "))"
  fromAST (Identifier name _) = name
  fromAST (Int i) = show i
  fromAST (Flt f) = show f
  fromAST (Return x) = "return " ++ fromAST x
  fromAST (Array xs t) = "(" ++ t ++ ") {" ++ (concat $ intersperse "," (map fromAST xs)) ++ "}"
  fromAST (Char c) = show c
  fromAST (Condition c t e) = fromAST c ++ " ? " ++ fromAST t ++ " : " ++ fromAST e
  fromAST x = show x

  addTemporary :: Compiler m => String -> m String
  addTemporary t = (modify $ \c -> c { temporary = temporary c ++ [t] }) >> return t

  clearTemporary :: Compiler m => m ()
  clearTemporary = modify $ \c -> c { temporary = [] }

  addLambdaType :: Compiler m => LambdaType -> m ()
  addLambdaType s = modify $ \c -> c { lambdaTypes = s : lambdaTypes c }

  addFunction :: Compiler m => CAST -> m ()
  addFunction s = modify $ \c -> c { functions = s : functions c }

  incCounter :: Compiler m => m String
  incCounter = do
    i <- gets lambdaCount
    modify $ \c -> c { lambdaCount = i + 1 }
    return $ lambdaPrefix ++ show i ++ "_t"

  addEnv :: Compiler m => EnvVariable -> m ()
  addEnv x = modify $ \c -> c { environment = x : environment c }

  searchEnv :: Compiler m => String -> m (Maybe EnvVariable)
  searchEnv x = gets environment >>= return . find (\(n,_, _) -> n == x)

  cleanEnv :: Compiler m => m ()
  cleanEnv = modify $ \c -> c { environment = [] }

  changeEnv :: Compiler m => [EnvVariable] -> m ()
  changeEnv e = modify $ \c -> c { environment = e }

  capitalize (x:xs) = toUpper x : xs

  type Compiler m = (MonadState C m, MonadIO m, MonadFail m)

  compile :: Compiler m => TypedAST -> m CAST
  compile (Node (Literal "begin" _) xs _) = do
    env <- gets environment
    body <- foldl (\acc -> \case
      Spread xs -> acc ++ xs
      x -> acc ++ [x]) [] <$> mapM compile xs
    changeEnv env
    return $ Block (if length body == 1 then [head body] else body)

  compile (Node (Literal "let" _) [Literal name t, value] _) = do
    t' <- createLambdaTypes t False
    v <- compile value
    addEnv (name, t', "")
    return $ Declaration (Identifier name t') v

  compile (Node (Literal "if" _) [cond, then', else'] _) = do
    cond  <- compile cond
    then' <- compile then'
    else' <- compile else'
    return $ Condition cond then' else'

  compile (Node (Literal "make-closure" _) (Literal name t:xs) _) = do
    t1 <- createLambdaTypes t True
    struct <- mapM (\x@(Literal n t) -> do
           value <- compile x
           return $ Assignment (Identifier (name ++ "_s." ++ n) t1) value) xs
    addEnv (name, t1, "")
    return $ Spread $ struct ++ [Identifier name t1]

  compile (Node (Literal "+" _) [x, y] _) = do
    x' <- compile x
    y' <- compile y
    return $ BinaryOperation "+" x' y'

  compile (Node (Literal "=" _) [x, y] _) = do
    x' <- compile x
    y' <- compile y
    return $ BinaryOperation "==" x' y'

  compile (Node (Literal "-" _) [x, y] _) = do
    x' <- compile x
    y' <- compile y
    return $ BinaryOperation "-" x' y'

  compile (Node (Literal "*" _) [x, y] _) = do
    x' <- compile x
    y' <- compile y
    return $ BinaryOperation "*" x' y'

  compile (Node (Literal "printf" _) [format, x] _) = do
    format <- compile format
    x <- compile x
    return $ FunctionCall (Identifier "printf" "int") [format, x] "int"

  compile (Node (Literal "#print_int" _) [x] _) = do
    x <- compile x
    return $ FunctionCall (Identifier "printf" "int") [Identifier "\"%d\"" "int", x] "int"

  compile (Node (Literal "#print_chr" _) [x] _) = do
    x <- compile x
    return $ FunctionCall (Identifier "printf" "int") [Identifier "\"%c\"" "int", x] "int"

  compile (Node (Literal "#print_str" _) [x] _) = do
    x <- compile x
    return $ FunctionCall (Identifier "printf" "int") [Identifier "\"%s\"" "int", x] "int"

  compile (Node (Literal "list" _) xs t) = do
    xs <- mapM compile xs
    t' <- createLambdaTypes (case t of
      List x l -> if all isChar xs then List x (l + 1) else t
      _ -> t) False
    return $ Array (if all isChar xs then xs ++ [Char 0] else xs) t'

  compile (Node (Literal "chr" _) [Integer f _] _) = return $ Char $ fromInteger f

  -- function call
  compile (Node n xs t) = do
    n'  <- compile n
    xs' <- mapM compile xs
    t' <- createLambdaTypes t True
    return $ FunctionCall n' xs' t'

  compile (Literal name t) =
    searchEnv name >>= return . \case
      Nothing -> error $ "No variable named " ++ name ++ " found in environment"
      Just (_, t', cl) -> Identifier (if not (null cl) then cl ++ "_s." ++ name else name) t'

  compile (Float f _) = return $ Flt f
  compile (Integer i _) = return $ Int $ fromInteger i
  compile x = do
    liftIO $ print x
    return $ Identifier "t" ""

  isChar (Char _) = True
  isChar _ = False

  toInt :: Float -> Int
  toInt = round

  -- bool arg indicates if it is function pointer
  createLambdaTypes :: Compiler m => Type -> Bool -> m String
  createLambdaTypes (Lambda args ret) _ = do
    lambdaName <- incCounter
    args' <- mapM (`createLambdaTypes` True) args
    ret'  <- createLambdaTypes ret True

    let start = "typedef " ++ ret' ++ " (*" ++ lambdaName ++ ")("
    addLambdaType $ start ++ concat (intersperse "," args') ++ ")"

    return lambdaName
  createLambdaTypes (List xs length) p = do
    x <- createLambdaTypes xs p
    return $ x ++ (if p then "*" else "[" ++ show length ++ "]")
  createLambdaTypes (Value "a") _ = return "void*"
  createLambdaTypes (Value x) _ = return x
  createLambdaTypes None _ = error "No support for variable without explicit type!"

  createEnvironment :: Compiler m => [Variable] -> String -> m ()
  createEnvironment env name = do
    xs' <- mapM (\(n, t) -> (n,) <$> createLambdaTypes t False) env
    mapM (\(n, t) -> addEnv (n, t, name)) xs'
    addLambdaType $ "struct " ++ capitalize name ++ " {" ++ (concat $ map (\(n, t) -> t ++ " " ++ n ++ ";") xs') ++ "}"
    addLambdaType $ "static struct " ++ capitalize name ++ " " ++ name ++ "_s"

  createFunction :: Compiler m => Closure -> m ()
  createFunction (Closure (name, t) env args body) = do
    n <- case name of
      "main" -> return "int"
      _ -> createLambdaTypes t True
    when (length env > 0) $ createEnvironment env name
    addEnv (name, n, "")

    env <- gets environment
    args' <- mapM (\x -> createLambdaTypes (snd x) False) args
    let a = replicate (length args) ""

    mapM (addEnv) (zip3 (map fst args) args' a)
    x <- compile body
    changeEnv env

    let body' = case x of
                  z@(Block xs) ->
                    let (els, ret) = (init xs, last xs)
                      in Block $ els ++ [Return ret]
                  Spread xs ->
                    let (els, ret) = (init xs, last xs)
                      in Block $ els ++ [Return ret]
                  _ -> Block [Return x]
    addFunction $ Function (Identifier name n) (map (\(x, y) -> Identifier x y) (zip (map fst args) args')) body'

  runCompiler :: (Monad m, MonadIO m, MonadFail m) => [Closure] -> m C
  runCompiler xs = execStateT (mapM createFunction (reverse xs)) (C [] [] [] 0 [])
