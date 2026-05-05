module Exterminator where

import Text.Megaparsec hiding (State, empty)




-- type IndentParser = ParsecT Void TokStream (State StackOp)


-- isBlank :: T Tok -> Bool
-- isBlank (T _ (TBlanks _)) = True
-- isBlank _ = False

-- -- exterminator, because I am removing terminating tokens.
-- -- like not really, I am removing blanks not linebreaks, but it sounded funny
-- whiteSpaceExterminator :: IndentParser [T Tok]
-- whiteSpaceExterminator = do
--   return ()
