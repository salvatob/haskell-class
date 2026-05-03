{-# LANGUAGE TypeFamilies #-}
module Lexer where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data OpTok
  = TPlusOp
  | TMinusOp
  | TStarOp
  | TSlashOp
  | TModOp
  | TLessOp
  | TGreaterOp
  deriving (Show, Eq, Ord)

{- | A data type for tokens. `TBlanks` stores the size of the blank space,
 - because we need it to measure the indentation width. -}
data Tok
  = TInt Int
  | TNewLine
  | TBlanks Int
  -- | TOp Char -- operators + - * / % < > 
  | TOp OpTok  
  | TLeftPar
  | TRightPar
  | TColon
  | TChar Char
  | TIdentifier String -- represents a variable or function name
  | TIf
  | TElse
  | TFuncDef
  | TPass
  deriving (Show, Eq, Ord)

-- | We use this function to show the tokens in error messages from the parser,
-- such as "unexpected TNewLine". If you want better error messages (such as
-- "unexpected line ending" :D ), modify this function first.
showTok :: Tok -> String
showTok = show

data T a = T
  { strT :: String -- ^ discards the data and returns the original string
  , unT :: !a -- ^ discards the wrap and returns the data
  } deriving (Show)

isNewLine (T _ TNewLine) = True
isNewLine _ = False

-- | The tokenizer eats normal Strings
type Tokenizer = Parsec Void String


tSimple :: Tok -> Char -> Tokenizer (T Tok)
tSimple t c = T [c] t <$ char c

tOp :: Tokenizer (T Tok)
tOp = choice 
  [ tSimple (TOp TPlusOp)     '+' 
  , tSimple (TOp TMinusOp)    '-'  
  , tSimple (TOp TStarOp)     '*' 
  , tSimple (TOp TSlashOp)    '/'  
  , tSimple (TOp TModOp)      '%' 
  , tSimple (TOp TLessOp)     '<'  
  , tSimple (TOp TGreaterOp)  '>'  
  ]

tCharLiteral :: Tokenizer (T Tok)
tCharLiteral = do
  char '\''
  c <- latin1Char
  char '\''
  return $ T [c] (TChar c)

tIdentifier :: Tokenizer (T Tok)
tIdentifier = do
  word <- some letterChar
  case word of
    "if"    -> pure $ T word TIf
    "else"  -> pure $ T word TElse
    "def"   -> pure $ T word TFuncDef
    "pass"  -> pure $ T word TPass
    _ ->  fail "token has not matched any known keyword"

tSymbol :: Tokenizer (T Tok)
tSymbol = do 
  word <- some letterChar
  return $ T word (TIdentifier word)


-- | This parses out all tokens (you might want to extend the token count)
tok :: Tokenizer (T Tok)
tok =
  choice
    [ try ((T <$> id <*> TInt . read) <$> some digitChar)
    , (T <$> id <*> TBlanks . length) <$> some (char ' ')
    , tSimple TNewLine '\n'
    , tSimple TLeftPar '('
    , tSimple TRightPar ')'
    , tSimple TColon ':'
    , tOp
    , try tCharLiteral
    , try tIdentifier
    , try tSymbol
    ]

toks :: Tokenizer [T Tok]
toks = many tok

-- | A nice wrapper for the token stream (we'll need to make instances on this,
-- so we want to have a type tag, not just a list alias).
newtype TokStream = TokStream
  { unTokStream :: [T Tok]
  } deriving (Show)

-- | This runs the tokenizer
tokenize = runParser (TokStream <$> toks <* eof)
