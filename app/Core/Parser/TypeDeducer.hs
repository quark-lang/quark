{-# LANGUAGE LambdaCase, BlockArguments, TupleSections #-}
module Core.Parser.TypeDeducer where
  import qualified Core.Parser.AST as A
  import Core.Parser.Macros
  import Control.Monad.RWS
  import Data.Maybe
  import qualified Data.Map as M
  import Data.List
  import Data.Functor
  import Debug.Trace (traceShow)
  import Data.Foldable

  -- typechecking transform a basic AST into a typed one
  data Type
    = Arrow Type Type
    | Var Int
    | String
    | Int
    | List Type
    | Void
    deriving Eq

  instance Show Type where
    show (Arrow ts t) = show ts ++ " ⟶ " ++ show t
    show (Var n) = "f" ++ show n
    show String = "𝕊"
    show Int = "ℕ"
    show (List t) = "[" ++ show t ++ "]"
    show Void = "∅"

  type Argument = (String, Type)
  data TypedAST
    = AppE TypedAST TypedAST Type
    | AbsE Argument TypedAST
    | VarE String Type
    | LetE Argument TypedAST TypedAST
    | ListE [TypedAST] Type
    | LitE A.AST
    deriving Eq

  instance Show TypedAST where
    show (AppE f args t) = "(" ++ show f ++ " " ++ show args ++ ")" ++ " :: " ++ show t
    show (AbsE args body) = "λ" ++ show args ++ ") -> " ++ show body
    show (VarE name t) = name ++ " :: " ++ show t
    show (LetE (name, t) body t') = "let " ++ name ++ " = " ++ show body ++ "\n  in " ++ show t'
    show (ListE args t) = "[" ++ show args ++ "]" ++ " :: " ++ show t
    show (LitE ast) = show ast

  -- Scheme represents a polymorphic type
  data Scheme = Scheme [Int] Type
    deriving (Show, Eq)

  -- some state related useful types
  type Variable = (String, Scheme)
  type Environment = M.Map String Scheme
  type Deducer m = (MonadRWS Environment [(Type, Type)] Int m, MonadIO m)

  -- tell the typechecker an equal type tuple
  (<=>) :: Deducer m => Type -> Type -> m ()
  a <=> b = tell [(a, b)]

  -- generate a fresh variable type
  fresh :: Deducer m => m Type
  fresh = do
    n <- get
    modify (+1)
    return $ Var n

  -- build constraints tree from AST
  constraint :: Deducer m => A.AST -> m TypedAST
  constraint (A.Node (A.Literal "fn") (arg:body:_)) = error "test"
    
  constraint _ = error "Not implemented"

  builtins =
    [
      ("+", Scheme [] (Arrow Int (Arrow Int Int))),
      ("begin", Scheme [0] (Arrow (List (Var 0)) (Var 0))),
      ("Nil", Scheme [] Void)
    ]

  generate :: (Monad m, MonadIO m) => A.AST -> m TypedAST
  generate a = runRWST (constraint a) (M.fromList builtins) 0 >>= \(x, a', b) -> do
    liftIO $ print "test"
    return x