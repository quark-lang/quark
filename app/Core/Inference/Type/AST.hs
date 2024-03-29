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
    -- (Name, [Generics])
    | DataE (String, [Type]) [(String, Type)]
    -- Pattern | [(Case, AST)]
    | PatternE TypedAST [(TypedPattern, TypedAST)]
    deriving Eq

  buildData :: Type -> [Type] -> Type
  buildData n xs = go n $ reverse xs
    where go name (x:xs) = TApp (go name xs) x
          go n [] = n

  getType :: TypedAST -> Type
  getType (AppE _ _ t) = t
  getType (AbsE _ t) = getType t
  getType (VarE _ t) = t
  getType (LetInE _ _ t) = getType t
  getType (ListE _ t) = t
  getType (LetE _ t) = getType t
  getType (LitE _ t) = t
  getType (DataE (n, ts) _) = buildData (TId n) ts
  getType (PatternE _ ((_, x):_)) = getType x
  getType _ = error "getType: not a valid type"

  data Class = IsIn String [Type]
    deriving (Eq, Ord)

  data Type
    = TVar Int | TId String
    | [Type] :-> Type
    | [Class] :=> Type
    | Int | String | Float | Bool | Char
    | TApp Type Type
    | ListT Type
    deriving (Eq, Ord)

  data Env = Env TypeEnv ConsEnv

  type SubTy   = Map Int Type
  type TypeEnv = Map String Scheme
  type ConsEnv = TypeEnv

  data Scheme = Forall [Int] Type
    deriving (Eq, Ord)

  type InstanceName = String
  type Instance = ([Class], (InstanceName, [Class]))
  type Instances = [Instance]
  type MonadType m = (MonadRWS Env () (Int, Instances) m, MonadIO m, MonadError (String, Expression) m)