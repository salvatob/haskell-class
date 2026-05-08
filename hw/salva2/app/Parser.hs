-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Lexer
import Text.Megaparsec hiding (match)
import Debug.Trace (trace)


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
  | SFuncDef Identifier [Identifier] Stmt
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
  first <- try pExpr
  rest <- many (single TComma *> pExpr)
  single TRightPar
  return $ EFuncCall name (first:rest)

-- pFuncDef :: Parser Stmt
-- pFuncDef = do





-- | parse a single integer
pInt :: Parser Int
pInt = do
  {- the line below carries an additional label for error messages (this allows
   - the parser to print stuff like "expected an integer") -}
  TInt i <- satisfy isInt <?> "an integer"
  return i
  where
    isInt (TInt _) = True
    isInt _ = False

pAtom :: Parser Expr
pAtom =
  (ELit <$> pInt)                 -- number literal
    <|> (do EVar <$> pIdentifier) -- or a variable
    <|> try pFuncCall             -- or a result of a function call


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
  first <- pAtom <?> "an integer"
  rest <- many ((,) <$> pOp <*> pAtom)
  return $ foldl applyOp first rest
  where
    applyOp acc (op, val) = EBinOp op acc val

pTopExpr :: Parser Stmt
pTopExpr = do
  e <- pExpr
  return $ SExpr e

pIf :: Parser Stmt
pIf = do
  single TIf <?> "here if 125"
  cond <- pExpr
  single TColon
  single TNewLine
  single TIndent 
  block <-  pBlock
  single TDedent
  
  return $ SIf cond block 
  <?> "an if statement 133"

pIfElse :: Parser Stmt
pIfElse = do   
  single TIf <?> "here if 125"
  cond <- pExpr
  single TColon
  single TNewLine
  single TIndent
  block1 <- pBlock
  trace "got here1" single TNewLine
  trace "got here2" single TDedent
  single TElse
  single TColon
  single TIndent 

  block2 <- pBlock
  single TDedent

  return $ SIfElse cond block1 block2


pAssignment :: Parser Stmt
pAssignment = do
  ident <- pIdentifier
  single TAssign
  expr <- pExpr
  return $ SAssign ident expr

pPass :: Parser Stmt
pPass = do SPass <$ single TPass

-- intentionally avoid parsing a block
parseStmt :: Parser Stmt
parseStmt = cleanupNL $ choice
  [ fail "unreachable"
  , try pAssignment
  -- , try pTopExpr
  -- , try pIf <?> "here if 163"
  , try pIfElse <?> "in IF-ELSE statement"
  , pPass 
  ]


pBlock :: Parser Stmt
pBlock = do
  statements <- many parseStmt
  return $ SBlock statements



runP :: String -> TokStream -> Either (ParseErrorBundle TokStream Void) Stmt
runP = runParser (pBlock <* eof)

