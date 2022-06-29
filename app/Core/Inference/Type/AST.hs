module Core.Inference.Type.AST where
  import Core.Parser.AST (Expression)
  import Data.Map (Map)
  import Control.Monad.RWS (MonadIO, MonadRWS)
  import Control.Monad.Except (MonadError)
  
  type Argument = (String, Type)
  data TypedPattern
    = VarP String Type
    | LitP Literal Type
    | WilP Type
    | AppP String [TypedPattern] Type
    deriving Eq

  getTypeP :: TypedPattern -> Type
  getTypeP (VarP _ t) = t
  getTypeP (LitP _ t) = t
  getTypeP (WilP t) = t
  getTypeP (AppP _ _ t) = t

  data Literal
    = I Integer
    | F Float
    | C Char
    | S String
    deriving Eq

  data TypedAST
    = AppE TypedAST [TypedAST] Type
    | AbsE [Argument] TypedAST
    | VarE String Type
    | LetInE Argument TypedAST TypedAST
    | ListE [TypedAST] Type
    | LetE Argument TypedAST
    | LitE Literal Type
    | IfE TypedAST TypedAST TypedAST
    -- (Name, [Generics])
    | DataE (String, [Type]) [(String, Type)]
    -- Pattern | [(Case, AST)]
    | PatternE TypedAST [(TypedPattern, TypedAST)]
    deriving Eq

  getType :: TypedAST -> Type
  getType (AppE _ _ t) = t
  getType (AbsE _ t) = getType t
  getType (VarE _ t) = t
  getType (LetInE _ _ t) = getType t
  getType (ListE _ t) = t
  getType (LetE _ t) = getType t
  getType (LitE _ t) = t
  getType (IfE _ t _) = getType t
  getType (DataE (n, ts) _) = TApp (TId n) ts
  getType (PatternE _ ((_, x):_)) = getType x
  getType _ = error "getType: not a valid type"

  data Type
    = TVar Int | TId String
    | [Type] :-> Type
    | Int | String | Float | Bool | Any
    | TApp Type [Type]
    | ListT Type
    deriving (Eq, Ord)

  data Env = Env TypeEnv ConsEnv

  type SubTy   = Map Int Type
  type TypeEnv = Map String Scheme
  type ConsEnv = TypeEnv

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord)

  type MonadType m = (MonadRWS Env () Int m, MonadIO m, MonadError [(String, Expression)] m)