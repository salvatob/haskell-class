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
  EVar v -> prettyName v
  EBinOp op l r ->
    -- Add parentheses around left/right if they have lower precedence
    -- (simplified: always wrap if needed, but here we keep it minimal)
    prettyExpr l <+> prettyOp op <+> prettyExpr r
  EUnaryOp Neg e -> "-" <> prettyExpr e
  EFuncCall (Identifier name) args -> pretty name <> tupled (prettyExpr <$> args)

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

prettyStmt n (SIf cond thenStmt) =  "if" <+> parens (prettyExpr cond) <+>  (prettyStmt (n+tab_width) thenStmt)
  -- vsep
  --   [ "if" <+> parens (prettyExpr cond)
  --   , indent tab_width (prettyStmt (n+tab_width) thenStmt)
  --   ]

prettyStmt n (SIfElse cond thenStmt elseStmt) =
  vsep
    [ "if" <+> parens (prettyExpr cond) <> "{"
    , indent tab_width (prettyStmt (n+tab_width) thenStmt)
    , "} else {"
    , indent tab_width (prettyStmt (n+tab_width) elseStmt)
    , "}"
    ]

prettyStmt n (SExpr expr) = prettyExpr expr

prettyStmt n (SBlock stmts) = "{" <+>
  vsep
    [ vsep  (map (indent tab_width . prettyStmt n) stmts)
    , "}"]

prettyStmt n (SFuncDef name params body) =
  vsep
    [ "function" <+> prettyName name <> tupled (prettyName <$> params) <+> "{"
      , indent tab_width (prettyStmt (n+tab_width) body)
      , "}"
    ]


prettyAST :: Program -> Doc ()
prettyAST s = vsep $ prettyStmt 0 <$> s


-- Render to a String with a default layout (80 columns)
printAST :: Program -> String
printAST = renderString . layoutPretty defaultLayoutOptions . prettyAST
