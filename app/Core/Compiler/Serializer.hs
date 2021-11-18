{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, EmptyCase #-}
module Core.Compiler.Serializer where
  import Core.Compiler.Instruction
  import Control.Monad.State
  import Data.List

  data Serialized = Serialized {
    blob :: [[Int]],
    constants :: [String]
  } deriving Show

  getBlob :: State Serialized [[Int]]
  getBlob = do
    Serialized blob _ <- get
    return blob

  getConstants :: State Serialized [String]
  getConstants = do
    Serialized _ constants <- get
    return constants

  addConstant :: String -> State Serialized Int
  addConstant constant = do
    Serialized blob constants <- get
    case elemIndex constant constants of
      Just index -> return index
      Nothing -> do
        let index = length constants
        put $ Serialized blob (constant:constants)
        return index

  addBlob :: [Int] -> State Serialized ()
  addBlob blob = do
    Serialized blobs constants <- get
    put $ Serialized (blobs ++ [blob]) constants

  class Serializer a where
    buildInstruction :: a -> State Serialized [Int]

    serialize :: [a] -> State Serialized Serialized
    serialize [] = return $ Serialized [] []
    serialize (x:xs) = do
      blob <- buildInstruction x
      addBlob blob
      serialize xs

  instance Serializer Instruction where
    buildInstruction (LOAD n) = do
      addr <- addConstant n
      return [0, addr]
    buildInstruction (STORE n) = do
      addr <- addConstant n
      return [1, addr]
    buildInstruction (DROP n) = do
      addr <- addConstant n
      return [2, addr]
    buildInstruction (PUSH n)      = return [3, n]
    buildInstruction ADD           = return [4, 0]
    buildInstruction (EXTERN n)    = return [5, n]
    buildInstruction HALT          = return [6, 0]
    buildInstruction (JUMP_ELSE n) = return [7, n]
    buildInstruction (JUMP_REL  n) = return [8, n]
    buildInstruction _ = return [-1, 0]
    

  runSerializer :: [Instruction] -> Serialized
  runSerializer instructions = execState (serialize instructions) (Serialized [] [])

