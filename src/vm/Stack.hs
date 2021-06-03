{-# LANGUAGE BlockArguments #-}
module VM.Stack where
  import Control.Monad.State
  import Core.Compiler hiding (Program)
  import Data.List
  import qualified Useful.Dictionary as D

  type Program = (Stack, [Table])

  spanFilter p s = (filter p s, filter (not . p) s)
  
  -- Symbols implementation

  pushTable :: StateT Program IO ()
  pushTable = state \(st,sy) -> ((), (st, D.dict [] : sy))

  popTable :: StateT Program IO Table
  popTable = state \(st,x:xs) -> (x, (st, xs))

  pushVariable :: Variable -> StateT Program IO ()
  pushVariable val = do
    table <- popTable
    (st,tables) <- get
    let updated = table D.#+ val
    put (st, updated : tables)

  popVariable :: String -> StateT Program IO Variable
  popVariable x = do
    table <- popTable
    (st,tables) <- get
    var <- getVariable x
    let updated = table D.#- x
    put (st, updated : tables)
    return var

  getVariable :: String -> StateT Program IO Variable
  getVariable n = do
    (_,tables) <- get
    let (table:_) = tables
    return (n, table D.#!! n)
  

  table :: StateT Program IO Table
  table = state \p@(_,x:xs) -> (x, p)

  initProgram' :: Program
  initProgram' = ([], [D.dict []])

  -- Frame implementation

  pushFrame :: StateT Program IO ()
  pushFrame = state \(st,sy) -> ((), ([] : st, sy))

  popFrame :: StateT Program IO Frame
  popFrame = state \(x:xs, sy) -> (x, (xs, sy))

  frame :: StateT Program IO Frame
  frame = state \z@(x:_,_) -> (x, z)

  -- Value implementation

  pushValue :: Value -> StateT Program IO ()
  pushValue val = do
    fr <- popFrame
    (st,sy) <- get
    let updated = val : fr
    put (updated : st, sy)

  popValue :: StateT Program IO Value
  popValue = do
    fr <- popFrame
    (st,sy) <- get
    let (x:xs) = fr
    put (xs : st, sy)
    return x
    