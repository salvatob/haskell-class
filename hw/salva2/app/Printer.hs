{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Printer where


import Prettyprinter          -- main library
import Prettyprinter.Render.String (renderString)
import Parser

-- Convert an expression to a Doc with annotation type ()
prettyExpr :: Expr -> Doc ()
prettyExpr = \case
  ELit n      -> pretty n
  EVar (Identifier v) -> pretty v
  EBinOp op l r ->
    -- Add parentheses around left/right if they have lower precedence
    -- (simplified: always wrap if needed, but here we keep it minimal)
    prettyExpr l <+> prettyOp op <+> prettyExpr r
  EUnaryOp Neg e -> "-" <> prettyExpr e

prettyOp :: BinOp -> Doc ()
prettyOp = \case
  Add    -> "+"
  Sub    -> "-"
  Mul    -> "*"
  Div    -> "/"
  Mod    -> "%"
  Less   -> "<"
  Greater -> ">"

-- Indentation level (number of spaces) is passed as Int, but we use `indent` from the library
prettyStmt :: Int -> Stmt -> Doc ()
prettyStmt _ SPass = "pass"

prettyStmt n (SAssign (Identifier var) expr) =
  pretty var <+> "<-" <+> prettyExpr expr

prettyStmt n (SIf cond thenStmt) =
  vsep
    [ "if" <+> (parens (prettyExpr cond)) <+> "{"
    , indent 2 (prettyStmt (n+2) thenStmt)
    , "}"
    ]

prettyStmt n (SIfElse cond thenStmt elseStmt) =
  vsep
    [ "if" <+> parens (prettyExpr cond) <> "{"
    , indent 2 (prettyStmt (n+2) thenStmt)
    , "} else {"
    , indent 2 (prettyStmt (n+2) elseStmt)
    , "}"
    ]

prettyStmt n (SExpr expr) = prettyExpr expr

prettyStmt n (SBlock stmts) =
  vsep (map (prettyStmt n) stmts)

prettyAST :: Stmt -> Doc ()
prettyAST = prettyStmt 0

-- wrapWith :: String -> String -> Doc ann -> Doc ann
-- wrapWith l r doc = text l <> doc <> (text r)

-- parentheses = wrapWith "(" ")"

-- parens :: Doc ann -> Doc ann
-- parens doc = char '(' <> doc <> char ')'

-- braces :: Doc ann -> Doc ann
-- braces doc = char '{' <> doc <> char '}'

-- Render to a String with a default layout (80 columns)
printAST :: Stmt -> String
printAST = renderString . layoutPretty defaultLayoutOptions . prettyAST
