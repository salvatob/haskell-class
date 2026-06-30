module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Numset = Set.Set Int




readAdd :: Numset -> IO (Numset)
-- readAdd :: Int -> IO Int
readAdd s = do
  line <- getLine
  let num = read line :: Int

  let newS = Set.insert num s
  -- let newS = num + s
  print newS

  readAdd newS


-- main :: IO ()
main = readAdd (Set.empty :: Numset)
-- main = readAdd 0
