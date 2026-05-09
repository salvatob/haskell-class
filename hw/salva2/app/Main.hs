{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where
import Lexer
import Parser
import StackState
import Printer
import Text.Megaparsec


doFile :: IO ()
doFile = do
  -- Get input file name from command line arguments, default to "input.txt"
  -- args <- getArgs
  -- let inputFile = if null args then "input.txt" else head args
  
  let inputFile = "input.txt"
  -- let inputFile = "good_input.txt"

  -- Read the source code
  source <- readFile inputFile

  -- Step 1: Tokenize
  putStrLn "Tokens incoming:"
  case tokenize inputFile source of
    Left err -> putStrLn $ errorBundlePretty err
    Right tokStream -> do
      let tokens = unT <$> unTokStream tokStream
      print tokens  -- print the list of tokens (without T wrapper)

      -- Step 2: Parse the token stream into an AST
      putStrLn "\nAST incoming:"
      case runP inputFile tokStream of
        Left parseErr -> putStrLn $ errorBundlePretty parseErr
        Right ast -> do
          putStrLn $ printAST ast
          putStrLn "\nParse successful!"


main :: IO ()
main = doFile

