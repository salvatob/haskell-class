module StackState where

import Control.Monad.State


type Stack = [Int]
type StackOp a = State Stack a

empty :: StackOp Bool
empty = do
  items <- get
  return (items==[])

push :: Int -> StackOp ()
push x = modify (x :)

pop :: StackOp (Maybe Int)
pop = do
  items <- get
  case items of
      (x:xs)-> do
        put xs
        return (Just x)
      [] -> return Nothing


testStack :: Stack -> IO ()
testStack initialStack = do
  line <- getLine
  let val = read line :: Int
  let (_, curr) = runState (push val) initialStack
  print curr
  testStack curr
  return ()
