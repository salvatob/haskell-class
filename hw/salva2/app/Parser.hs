{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Monad (void)
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
