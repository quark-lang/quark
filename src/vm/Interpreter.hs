{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module VM.Interpreter where
  import VM.Stack
  import qualified Core.Compiler as Compiler
  import Control.Monad.State
  import Control.Monad (void)
  import System.IO.Unsafe
  import qualified Useful.Dictionary as D

  isInt x = x == fromInteger (round x)

  revList [] = []
  revList (x:xs) = revList xs ++ [x]
  
  popNTimes :: Int -> StateT Program IO [Compiler.Value]
  popNTimes n = do
    (st, sy) <- get
    let (x:xs) = st
    let args = take n x
    let rest = drop n x
    let new  = rest : xs

    put (new, sy)
    return args
  
  instruction :: Compiler.Bytecode -> [Compiler.Page] -> StateT Program IO ()
  instruction (Compiler.PUSH v) p = pushValue v
  instruction (Compiler.DROP x) p = void $ popVariable x
  instruction (Compiler.LOAD_SEGMENT i) p = do
    let page = p !! i
    f <- frame
    t <- table
    pushValue $ Compiler.VCode page (f, t)
  instruction (Compiler.LOAD "*") p = do
    pushValue $ Compiler.VCode [Compiler.MUL] ([], D.dict [])
  instruction (Compiler.STORE x) p = do
    val <- popValue
    pushVariable (x, val)
  instruction (Compiler.LOAD x) p = do
    (_,val) <- getVariable x
    pushValue val
  
  instruction (Compiler.LIST i) p = do
    elements <- revList <$> popNTimes i
    pushValue $ Compiler.VList elements

  instruction (Compiler.PRINT x) p = do
    args <- revList <$> popNTimes x
    mapM_ (liftIO . putStr . (++" ") . show) args
    liftIO $ putStrLn ""
    return ()

  instruction Compiler.SUB p = do
    args <- revList <$> popNTimes 2
    let (a:b:_) = args 
    pushValue $ case a of

      Compiler.VInteger a' -> case b of
        Compiler.VInteger b' -> Compiler.VInteger $ a' - b'
        Compiler.VDouble b' -> Compiler.VDouble $ fromIntegral a' - b'

      Compiler.VDouble a' -> case b of
        Compiler.VInteger b' -> Compiler.VDouble $ a' - fromIntegral b'
        Compiler.VDouble b' -> Compiler.VDouble $ a' - b'

  instruction Compiler.MUL p = do
    args <- revList <$> popNTimes 2
    let (a:b:_) = args 
    pushValue $ case a of

      Compiler.VInteger a' -> case b of
        Compiler.VInteger b' -> Compiler.VInteger $ a' * b'
        Compiler.VDouble b' -> Compiler.VDouble $ fromIntegral a' * b'
        
      Compiler.VDouble a' -> case b of
        Compiler.VInteger b' -> Compiler.VDouble $ a' * fromIntegral b'
        Compiler.VDouble b' -> Compiler.VDouble $ a' * b'

  instruction Compiler.ADD p = do
    args <- revList <$> popNTimes 2
    let (a:b:_) = args 
    pushValue $ case a of

      Compiler.VInteger a' -> case b of
        Compiler.VInteger b' -> Compiler.VInteger $ a' + b'
        Compiler.VDouble b' -> Compiler.VDouble $ fromIntegral a' + b'
        
      Compiler.VDouble a' -> case b of
        Compiler.VInteger b' -> Compiler.VDouble $ a' + fromIntegral b'
        Compiler.VDouble b' -> Compiler.VDouble $ a' + b'

  instruction (Compiler.CALL x) p = do
    args <- popNTimes x
    func <- popValue 
    case func of 
      Compiler.VCode f _ -> do
        st <- table

        pushTable
        pushFrame 

        mapM_ pushVariable (D.dictToList st)
        mapM_ pushValue args

        _ <- return $ unsafePerformIO $ print "test"

        mapM_ (`instruction` p) f
        ret <- popValue 

        popFrame
        popTable

        pushValue ret
      _ -> error "Can't call a value as a function!"

  instruction Compiler.CMP_EQ p = do
    a <- popValue
    b <- popValue
    pushValue $ Compiler.VBool (b == a)
  instruction (Compiler.JUMP i j) p = do
    a <- popValue
    case a of
      Compiler.VBool x -> let page = p !! (if x then i else j) in
        mapM_ (`instruction` p) page
      _ -> error "Can't jump due to bug!"
  instruction _ _ = return ()

  run :: [Compiler.Page] -> StateT Program IO ()
  run z@(x:xs) = do
    pushFrame
    mapM_ (`instruction` z) x
    