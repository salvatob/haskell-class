module Exterminator where

import Control.Monad.State.Strict
import Text.Megaparsec hiding (State)

-- data IndentState = IndentState { indentStack :: [Int] }

-- push :: IndentState -> Int -> IndentState
-- push 

newType Stack = State [Int] a



type IndentParser = ParsecT Void TokStream (State IndentState)



isBlank :: T Tok -> Bool
isBlank (T _ (TBlanks _)) = True
isBlank _ = False

-- exterminator, because I am removing terminating tokens.
-- like not really, I am removing blanks not linebreaks, but it sounded funny
whiteSpaceExterminator :: IndentParser [T Tok]
whiteSpaceExterminator = do
  return ...