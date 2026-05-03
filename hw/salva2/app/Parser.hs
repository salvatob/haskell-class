{-# LANGUAGE TypeFamilies #-}

module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Lexer
import Text.Megaparsec


newtype Identifier = Identifier String
  deriving (Show, Eq, Ord)

data Expr
  = EVar Identifier
  | ELit Int
  | EBinOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  deriving (Show)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Less | Greater
  deriving (Show)

data UnaryOp
  = Neg
  deriving (Show)

data Stmt
  = SBlock [Stmt]
  | SAssign Identifier Expr
  | SIf Expr Stmt
  | SIfElse Expr Stmt Stmt
  | SExpr Expr
  | SPass
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

pAtom :: Parser Expr
pAtom = pLexeme $
  (ELit <$> pInt)
    <|> (do
           TIdentifier var <- satisfy isIdentifier
           return $ EVar $ Identifier var)
  where
    isIdentifier (TIdentifier _) = True
    isIdentifier _ = False


pOp :: Parser BinOp
pOp = do
  TOp op <- satisfy isOp
  pure
    $ case op of
        TPlusOp -> Add
        TMinusOp -> Sub
        TStarOp -> Mul
        TSlashOp -> Div
        TModOp -> Mod
        TLessOp -> Less
        TGreaterOp -> Greater
  where
    isOp (TOp _) = True
    isOp _ = False


pExpr :: Parser Expr
pExpr = do
  first <- pAtom
  rest <- many ((,) <$> pOp <*> pAtom)
  return $ foldl applyOp first rest
  where
    applyOp acc (op, val) = EBinOp op acc val


pBlock :: Parser Stmt
pBlock = do
  blanks
  satisfy isPass
  return SPass
  where
    isPass TPass = True
    isPass _ = False


pIf :: Parser Stmt
pIf = do
  satisfy isIf
  blanks
  expr <- pExpr
  satisfy isColon
  SIf expr <$> pBlock
  where
    isIf TIf = True
    isIf _ = False
    isColon TColon = True
    isColon _ = False



parseExpr = runParser (pExpr <* eof)
parseIf = runParser (pIf <* eof)

-- | eat 2 integers and a newline
pairLine = (,) <$> pLexeme pInt <*> pLexeme pInt <* pLexeme (single TNewLine)

-- | parse lots of integer pairs, each on a single line
parsePairs = runParser (blanks *> many pairLine <* eof)
