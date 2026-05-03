{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Monad (void)
import Data.Bool (bool)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Void (Void)
import Lexer
import Text.Megaparsec

data Expression
  = Variable String
  | Literal Int
  | Plus Expression Expression
  | Minus Expression Expression
  | Times Expression Expression
  | Div Expression Expression
  | Mod Expression Expression
  | Less Expression Expression
  | Greater Expression Expression
  deriving (Show)

data Node
  = Block [Node]
  | Symbol String
  | Assignment 
  | If Expression Node
  | IfElse Expression Node Node
  deriving (Show)

-- | The parser takes the tagged stream of tokens as an input
type Parser = Parsec Void TokStream

-- | parse any amount of blanks
blanks :: Parser ()
blanks = void $ many (satisfy isBlank)
  where
    isBlank (TBlanks _) = True
    isBlank _ = False

-- | eat blanks after a given parse
pLexeme :: Parser a -> Parser a
pLexeme = (<* blanks)

match :: (Tok -> Maybe a) -> Parser a
match f = do
  t <- satisfy (maybe False (const True) . f)
  case f t of
    Just x  -> pure x
    Nothing -> fail "unreachable"


-- | parse a single integer
pInt :: Parser Int
pInt = pLexeme $ do
  {- the line below carries an additional label for error messages (this allows
   - the parser to print stuff like "expected an integer") -}
  TInt i <- satisfy isInt <?> "an integer"
  return i
  where
    isInt (TInt _) = True
    isInt _ = False

pAtom :: Parser Expression
pAtom = pLexeme $
  (Literal <$> pInt)
    <|> (do
           TIdentifier var <- satisfy isIdentifier
           return $ Variable var)
  where
    isIdentifier (TIdentifier _) = True
    isIdentifier _ = False


pOp :: Parser (Expression -> Expression -> Expression)
pOp = do
  TOp op <- satisfy isOp
  pure
    $ case op of
        TPlusOp -> Plus
        TMinusOp -> Minus
        TStarOp -> Times
        TSlashOp -> Div
        TModOp -> Mod
        TLessOp -> Less
        TGreaterOp -> Greater
  where
    isOp (TOp _) = True
    isOp _ = False

pExpr :: Parser Expression
pExpr = do
  first <- pAtom
  rest <- many ((,) <$> pOp <*> pAtom)
  pure $ foldl (\acc (op, val) -> op acc val) first rest

pBlock :: Parser Node
pBlock = do
  blanks
  p <- satisfy isPass
  return $ Symbol
  where
    isPass TPass = True
    isPass _ = False

pIf :: Parser Node
pIf = do
  _ <- satisfy isIf
  expr <- pExpr
  _ <- satisfy isColon
  body <- pBlock
  return $ If expr body
  where
    isIf TIf = True
    isIf _ = False
    isColon TColon = True
    isColon _ = False



parseExpr = runParser (pExpr <* eof)

-- | eat 2 integers and a newline
pairLine = (,) <$> pLexeme pInt <*> pLexeme pInt <* pLexeme (single TNewLine)

-- | parse lots of integer pairs, each on a single line
parsePairs = runParser (blanks *> many pairLine <* eof)

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
