{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Core.Compiler.Compiler where
  import Core.Parser.AST
  import Control.Monad.State
  import GHC.Float (float2Double)
  import Core.Parser.Macros (unliteral)
  import Data.Functor ((<&>))
  import Core.Parser.Utils.ClosureConversion
  import Core.Compiler.Instruction
  import Control.Monad.State (evalStateT)

  data Lambda = Lambda {
    pointer :: Int,
    variables :: [(String, Int)]
  } deriving Show

  type LambdaState m = (MonadState Lambda m, MonadIO m)

  addPointer :: LambdaState m => String -> m Int
  addPointer n = do
    Lambda p vars <- get
    put $ Lambda (p + 1) ((n, p) : vars)
    return p

  getPointer :: LambdaState m => String -> m Int
  getPointer n = do
    Lambda p vars <- get
    case lookup n vars of
      Just p' -> return p
      Nothing -> liftIO (print n) >> return (-1)

  toInt :: Float -> Int
  toInt = round

  startsWith :: String -> String -> Bool
  startsWith a b = a == take (length a) b

  removeStringFromString :: String -> String -> String
  removeStringFromString a = drop (length a)

  compileLambda :: LambdaState m => ClosuredNode -> Int -> m Section
  compileLambda (Closure name args (env, body)) i = do
    mapM_ addPointer $ args ++ env
    Section i <$> helper body
    where helper :: LambdaState m => AST -> m [Instruction]
          helper (Node (Literal "let") [Literal name, value]) = do
            addr <- addPointer name
            v <- helper value
            return $ v ++ [STORE addr]
          helper (Node (Literal "begin") xs) = concat <$> mapM helper xs
          helper (Node (Literal "print") [value]) = helper value <&> (++ [EXTERN 0])
          helper (Node (Literal "input") [value]) = helper value <&> (++ [EXTERN 1])
          helper (Node (Literal "=") [lhs, rhs]) = do
            lhs' <- helper lhs
            rhs' <- helper rhs
            return $ lhs' ++ rhs' ++ [EXTERN 2]
          helper (Node (Literal "-") [lhs, rhs]) = do
            lhs' <- helper lhs
            rhs' <- helper rhs
            return $ lhs' ++ rhs' ++ [EXTERN 3]
          helper (Node (Literal "if") [cond, t, e]) = do
            c <- helper cond
            t' <- helper t
            e' <- helper e
            return $ c ++ [JUMP_ELSE (length t')] ++ t' ++ [JUMP_REL (length e')] ++ e'
          helper (Node (Literal "drop") [Literal n]) =
            if startsWith "lambda" n
              then return []
              else do
                addr <- getPointer n
                return [DROP addr]
            
          helper (Node n xs) = do
            n' <- helper n
            xs' <- mapM helper xs
            return $ n' ++ concat xs' ++ [CALL (length xs)]
          helper (Literal n) =
            if startsWith "lambda" n
              then return [LOAD_SECTION . read $ removeStringFromString "lambda" n]
              else do
                addr <- getPointer n
                return [LOAD addr]
          helper (Float f)   = return [PUSH $ toInt f]
          helper (Integer n) = return [PUSH $ fromInteger n]
          helper x = return []

  compile :: (Monad m, MonadIO m) => ClosuredAST -> m [Section]
  compile xs = do
    let data' = zip [0..] xs
      in mapM (\(i, x) -> evalStateT (compileLambda x i) (Lambda 0 [])) data'