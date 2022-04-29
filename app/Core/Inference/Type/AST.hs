module Core.Inference.Type.AST where
  import Core.Parser.AST (AST)
  import Data.Map (Map)
  import Core.Inference.Kind (KindEnv)
  import Control.Monad.RWS (MonadIO, MonadRWS)

  type Argument = (String, Type)
  data TypedAST
    = AppE TypedAST TypedAST Type
    | AbsE Argument TypedAST
    | VarE String Type
    | LetInE Argument TypedAST TypedAST
    | ListE [TypedAST] Type
    | LetE Argument TypedAST
    | LitE AST Type
    deriving Eq

  data Type
    = TVar Int | TId String
    | Type :-> Type
    | Int | String | Float
    | TApp Type Type
    deriving (Eq, Ord)

  data Env = Env TypeEnv ConsEnv KindEnv

  type SubTy   = Map Int Type
  type TypeEnv = Map String Scheme
  type ConsEnv = TypeEnv

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord)

  type MonadType m = (MonadRWS Env () Int m, MonadIO m)