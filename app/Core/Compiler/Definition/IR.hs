module Core.Compiler.Definition.IR where
  import Core.Inference.Type.AST
  import Core.Inference.Type.Pretty ()
  import qualified Data.Map as M
  import Control.Monad.State

  type Constructors = M.Map String String
  type MonadCompiler m = (MonadState Constructors m, Monad m, MonadIO m)

  data Expression
    -- Expressions
    = Lambda [String] Expression
    | Var String
    | Lit Literal
    | Object [(String, Expression)]
    | Ternary Expression Expression Expression
    | Array [Expression]
    | Index Expression Expression
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
    | Raw String
    deriving (Show, Eq)