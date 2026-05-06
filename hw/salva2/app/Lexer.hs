{-# LANGUAGE TypeFamilies #-}
module Lexer where

import Data.Void (Void)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import StackState
import Control.Monad.State



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
  -- | TBlanks Int
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

isNewLine :: T Tok -> Bool
isNewLine (T _ TNewLine) = True
isNewLine _ = False

-- | The tokenizer eats normal Strings
-- type Tokenizer = Parsec Void String
type Tokenizer = ParsecT Void String (State Stack)


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
    _ ->  fail $ "operator " ++ show word ++ " has not matched any known keyword"


tSymbol :: Tokenizer (T Tok)
tSymbol = do
  word <- some letterChar
  return $ T word (TIdentifier word)

tInt :: Tokenizer (T Tok)
tInt = try ((T <$> id <*> TInt . read) <$> some digitChar)

-- | This parses out all tokens (you might want to extend the token count)
tok :: Tokenizer (T Tok)
tok =
  choice
    [
    -- , (T <$> id <*> TBlanks . length) <$> some (char ' ')
    tSimple TNewLine '\n'
    , tSimple TLeftPar '('
    , tSimple TRightPar ')'
    , tSimple TColon ':'
    , tOp
    , try tCharLiteral
    , try tIdentifier
    , try tSymbol
    , tInt
    ]

toks :: Tokenizer [T Tok]
toks = many tok

-- | A nice wrapper for the token stream (we'll need to make instances on this,
-- so we want to have a type tag, not just a list alias).
newtype TokStream = TokStream
  { unTokStream :: [T Tok]
  } deriving (Show)

  -- run the stateful parser with initial stack [0]
tokenize :: String -> String -> Either (ParseErrorBundle String Void) TokStream
tokenize sourceName input =
  evalState (runParserT (TokStream <$> toks <* eof) sourceName input) [0]

-- tokenize = runParser (TokStream <$> toks <* eof)



-- | This is a megaparsec Stream instance for our `TokStream`, which works as
-- an adapter between our lists of labeled tokens and megaparsec. Essentially,
-- it tells megaparsec how to consume the TokStream. Similar instances exist
-- for String, Text, ByteString, and other parser-input types.
--
-- Essentially, megaparsec is able to parse anything as long as it has the
-- Stream instance defined.
instance Stream TokStream where
  type Token TokStream = Tok -- token type that the parsing function is interested in
  type Tokens TokStream = [Tok] -- type for "several tokens"
  tokenToChunk _ = (: []) -- some conversion functions
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokStream (x:xs)) = Just (unT x, TokStream xs) -- extracts a token
  take1_ _ = Nothing
  takeN_ n (TokStream l@(_:_)) = Just (map unT $ take n l, TokStream $ drop n l) -- extracts a chunk
  takeN_ _ _ = Nothing
  takeWhile_ f (TokStream l) =
    (map unT $ takeWhile (f . unT) l, TokStream $ dropWhile (f . unT) l)

-- | THIS BELOW you don't usually want to read.
--
-- This is the "rest" of the TokStream instances that is required for
-- megaparsec to be able to reconstruct good error messages from our TokStream.
--
-- In particular, whole the `Stream` instance tells megaparsec how to get
-- values out of the stream, the VisualStream tells it how to show the tokens
-- to the user in case some token is e.g. known to be expected but missing...
instance VisualStream TokStream where
  showTokens _ (a :| b) = intercalate ", " $ map showTok (a : b)

-- | ...and the TraversableStream instance tells it how to walk the token
-- stream in such a way that reconstructing a good "example source" of where
-- the error has occured is easy.
instance TraversableStream TokStream where
  reachOffset o pst =
    let (reachtoks, resttoks) =
          splitAt (o - pstateOffset pst) . unTokStream $ pstateInput pst
        (rln, rst) = break isNewLine (reverse reachtoks)
        linesFinished = length . filter isNewLine $ rst
        sameLine = linesFinished == 0
        line = unLine (reverse rln)
        rest = unLine (fst $ break isNewLine resttoks)
        unLine = unTab . concatMap strT
        unTab "" = ""
        unTab ('\t':cs) = replicate (unPos $ pstateTabWidth pst) ' ' ++ unTab cs
        unTab (c:cs) = c : unTab cs
        unempty "" = "<empty line>"
        unempty a = a
        pfx = bool id (pstateLinePrefix pst ++) sameLine
        sp = pstateSourcePos pst
        col = length line + bool 1 (unPos $ sourceColumn sp) sameLine
        row = unPos (sourceLine sp) + linesFinished
     in ( Just . unempty $ pfx line ++ rest
        , pst
            { pstateInput = TokStream resttoks
            , pstateOffset = o
            , pstateSourcePos =
                sp {sourceLine = mkPos row, sourceColumn = mkPos col}
            , pstateLinePrefix = pfx line
            })
