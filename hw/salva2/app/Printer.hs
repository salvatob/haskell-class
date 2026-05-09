{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer where

import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Parser

tab_width :: Int
tab_width = 4

prettyName :: Identifier -> Doc ann
prettyName (Identifier i) = pretty i

-- Convert an expression to a Doc with annotation type ()
prettyExpr :: Expr -> Doc ()
prettyExpr = \case
  ELit n      -> pretty n
  EVar v      -> prettyName v
  EBinOp op l r ->
    prettyExpr l <+> prettyOp op <+> prettyExpr r
  EUnaryOp Neg e -> "-" <> prettyExpr e
  EFuncCall (Identifier name) args ->
    pretty name <> tupled (prettyExpr <$> args)

prettyOp :: BinOp -> Doc ()
prettyOp = \case
  Add    -> "+"
  Sub    -> "-"
  Mul    -> "*"
  Div    -> "/"
  Mod    -> "%"
  Less   -> "<"
  Greater -> ">"


-- Statements: blocks print their own { and }, if/if-else delegate to the body
prettyStmt :: Stmt -> Doc ()
prettyStmt = \case
  SPass -> "pass" <> ";"

  SAssign (Identifier var) expr ->
    pretty var <+> "<-" <+> prettyExpr expr <> ";"

  SIf cond body ->
    "if" <+> parens (prettyExpr cond) <+> prettyStmt body
  
  SWhile cond body ->
    "while" <+> parens (prettyExpr cond) <+> prettyStmt body
      
  SIfElse cond thenBody elseBody ->
    "if" <+> parens (prettyExpr cond) <+> prettyStmt thenBody
    <+> "else" <+> prettyStmt elseBody

  SExpr expr ->
    prettyExpr expr <> ";"

  SBlock stmts ->
    "{" <> line <>
    indent tab_width (vsep (map prettyStmt stmts)) <>
    line <> "}"
    
  SFuncDef name params body ->
    "function" <+> prettyName name <>
    tupled (map prettyName params) <+> prettyStmt body

prettyAST :: Program -> Doc ()
prettyAST stmts = vsep (map prettyStmt stmts)

-- Render to a String with a default layout (80 columns)
printAST :: Program -> String
printAST = renderString . layoutPretty defaultLayoutOptions . prettyAST
