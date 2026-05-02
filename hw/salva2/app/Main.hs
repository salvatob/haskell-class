{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where
import Lexer
import Parser
import Text.Megaparsec


-- | a bit of demonstration
main :: IO ()
main = do
  let input = "'P'"
  let tokens = tokenize "input.txt" input
  print tokens

