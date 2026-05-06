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

peek :: StackOp (Maybe Int)
peek = do
  items <- get
  return $ case items of
    []    ->  Nothing
    (x:_) -> Just x

-- | pop until the top item is equal to 'width'
-- | if such item is present returns the number of popped items, else returns Nothing 
popToWidth :: Int -> StackOp (Maybe Int)
popToWidth w = do
  lastIt <- peek
  case lastIt of
    Nothing -> return Nothing
    Just level -> case compare level w of
      GT ->   do
        pop
        wid <- popToWidth w
        return $ Just (+1)  <*> wid
        
      EQ -> return (Just 0)
      LT -> return Nothing


testStack :: Stack -> IO ()
testStack initialStack = do
  line <- getLine
  let val = read line :: Int
  let (_, curr) = runState (push val) initialStack
  print curr
  testStack curr
  return ()
