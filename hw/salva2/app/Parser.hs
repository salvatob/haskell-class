{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use <$>" #-}

-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Debug.Trace (trace)
import Lexer
import Text.Megaparsec hiding (match)

newtype Identifier =
  Identifier String
  deriving (Show, Eq, Ord)

data Expr
  = EVar Identifier
  | ELit Int
  | EBinOp BinOp Expr Expr
  | EUnaryOp UnaryOp Expr
  | EFuncCall Identifier [Expr]
  deriving (Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | Greater
  deriving (Show)

data UnaryOp =
  Neg
  deriving (Show)

data Stmt
  = SBlock [Stmt]
  | SAssign Identifier Expr
  | SIf Expr Stmt
  | SWhile Expr Stmt
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
    Just x -> pure x
    Nothing -> fail "unreachable"

pIdentifier :: Parser Identifier
pIdentifier =
  match
    (\case
       TIdentifier s -> Just (Identifier s)
       _ -> Nothing)
    <?> "identifier"

pList :: Parser p -> Parser [p]
pList p = do
  first <- p
  rest <- many (single TComma *> p)
  return $ (first:rest)

pFuncCall :: Parser Expr
pFuncCall = do
  name <- pIdentifier
  pSimple TLeftPar
  args <- pList pExpr
  -- first <- pExpr
  -- rest <- many (single TComma *> pExpr)
  pSimple TRightPar
  return $ EFuncCall name args


pFuncDef :: Parser Stmt
pFuncDef = do
  pSimple TFuncDef
  name <- pIdentifier <?> "function name"
  pSimple TLeftPar
  params <- pList pIdentifier

  pSeq [TRightPar, TColon, TNewLine]

  body <- pBlock <?> "function body"
  return $ SFuncDef name params body


pSimple :: Tok -> Parser ()
pSimple t = void $ single t

-- TODO this may not correctly deal with errors
pSeq :: [Tok] -> Parser ()
pSeq = mapM_ single

-- | parse a single integer
pInt :: Parser Int
pInt = do
  {- the line below carries an additional label for error messages (this allows
   - the parser to print stuff like "expected an integer") -}
  TInt i <- satisfy isInt <?> "an integer literal"
  return i
  where
    isInt (TInt _) = True
    isInt _ = False

pAtom :: Parser Expr
pAtom =
   try pFuncCall -- or a result of a function call
    <|> (ELit <$> pInt) -- number literal 
    <|> ( EVar <$> pIdentifier) -- or a variable

pOp :: Parser BinOp
pOp = do
  TOp op <- satisfy isOp <?> "operator"
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

-- cleanupNL :: Parser Stmt -> Parser Stmt
cln :: Parser ()
-- cleanupNL :: 
cln = void $ some (single TNewLine)

-- | Parses a binary expression in infix form
-- This one absolutely disregards all operator precedence, but implementing 
-- a whole shunning yard algorithm to parse infix operators just doesn't sound that fun 
pExpr :: Parser Expr
pExpr = do
  first <- pAtom <?> "an integer"
  rest <- many ((,) <$> pOp <*> pAtom)
  return $ foldl applyOp first rest
  where
    applyOp acc (op, val) = EBinOp op acc val


-- | Parses an expression, without assingning the result. Mostly used for void func calls
pTopExpr :: Parser Stmt
pTopExpr = do
  e <- pExpr
  return $ SExpr e


pIf :: Parser Stmt
pIf = do
  pSimple TIf
  cond <- pExpr <?> "a condition expression"
  pSeq [TColon, TNewLine]
  body <- pBlock
  return $ SIf cond body
  <?> "an if statement"

pIfElse :: Parser Stmt
pIfElse = do
  pSimple TIf
  cond <- pExpr
  pSeq [TColon, TNewLine]
  block1 <- pBlock
  pSeq [TElse, TColon]
  pSimple TNewLine
  block2 <- pBlock
  return $ SIfElse cond block1 block2


pWhile :: Parser Stmt
pWhile = do
  pSimple TWhile
  cond <- pExpr <?> "a condition expression"
  pSimple TColon
  body <- parseStmt
  return $ SWhile cond body
  <?> "a while statement"


pAssignment :: Parser Stmt
pAssignment = do
  ident <- pIdentifier
  pSimple TAssign
  expr <- pExpr
  return $ SAssign ident expr

pPass :: Parser Stmt
pPass = do
  SPass <$ single TPass

-- intentionally avoid parsing a block
parseStmt :: Parser Stmt
parseStmt =
  choice
    [ try $ pAssignment <* cln
    , try (pTopExpr <* cln) <?> "top level expression (func call)"
    , pPass <* cln
    , pFuncDef
    , try pIfElse <?> "an IF-ELSE statement"
    , try pIf <?> "an if statement"
    , try pWhile
    , try pBlock
    ]

type Program = [Stmt]

pBlock :: Parser Stmt
pBlock = do
  pSimple TIndent
  -- statements <- many (parseStmt <* pSimple TNewLine) <?> "empty lines not possible in here"
  statements <- sepBy1 parseStmt (many (single TNewLine)) -- at least one newline between stmts
  _ <- many (single TNewLine)
  pSimple TDedent
  return $ SBlock statements

pProgram :: Parser Program
pProgram = do
    many (parseStmt <* many (pSimple TNewLine))
      -- <?> "empty lines not possible in here"
  

runP :: String -> TokStream -> Either (ParseErrorBundle TokStream Void) Program
runP = runParser (pProgram <* eof)
-- runP = runParser (pIfElse <* eof)
