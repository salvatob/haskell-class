-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Lexer
import Text.Megaparsec hiding (match)


newtype Identifier = Identifier String
  deriving (Show, Eq, Ord)

data Expr
  = EVar Identifier
  | ELit Int
  | EBinOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  | EFuncCall Identifier [Expr]
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


match :: (Tok -> Maybe a) -> Parser a
match f = do
  t <- satisfy (maybe False (const True) . f)
  case f t of
    Just x  -> pure x
    Nothing -> fail "unreachable"


pIdentifier :: Parser Identifier
pIdentifier = match (\case TIdentifier s -> Just (Identifier s); _ -> Nothing)

pFuncCall :: Parser Expr
pFuncCall = do
  name <- pIdentifier
  single TLeftPar

  single TRightPar

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

cleanupNL :: Parser Stmt -> Parser Stmt
cleanupNL p = p <* many (single TNewLine)

pExpr :: Parser Expr
pExpr = do
  first <- pAtom
  rest <- many ((,) <$> pOp <*> pAtom)
  return $ foldl applyOp first rest
  where
    applyOp acc (op, val) = EBinOp op acc val


pBlock :: Parser Stmt
pBlock = do
  -- blanks
  satisfy isPass
  return SPass
  where
    isPass TPass = True
    isPass _ = False


pIf :: Parser Stmt
pIf = do
  single TIf
  expr <- pExpr
  single TColon
  SIf expr <$> pBlock

pAssignment :: Parser Stmt
pAssignment = do
  ident <- pIdentifier
  single TAssign
  expr <- pExpr
  return $ SAssign ident expr

parseTokens = cleanupNL $ choice 
  [ pAssignment
  , pIf
  , pBlock
  ]

parseExpr = runParser (pExpr <* eof)
parseIf = runParser (pIf <* eof)

runP = runParser (parseTokens <* eof)

