{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where
import Lexer
import Parser
import Text.Megaparsec
import StackState

-- main = testStack [69]

-- | a bit of demonstration
main :: IO ()
main = do
  
  let inputFile = "input.txt"
  input <- readFile inputFile

  print "tokens incoming:"
  let tokens = tokenize inputFile input
  case tokens of
    Right t -> print t
    Left err -> putStrLn $ errorBundlePretty err

  print "AST incoming:"
  let ast =
        case tokens of
          Right t -> parseIf inputFile t
          -- Left err -> Left err

  case ast of
    Right tree -> print tree
    Left e -> putStrLn $ errorBundlePretty e
  
  return ()
