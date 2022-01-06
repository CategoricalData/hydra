-- | Defines a Haskell-like language ("HydraScript") for pretty printing Hydra terms and types
-- Haskell operator precendence and associativity are drawn from:
--   https://self-learning-java-tutorial.blogspot.com/2016/04/haskell-operator-precedence.html
-- Other operators were investigated using GHCi, e.g. ":info (->)"
-- Operator names are drawn (loosely) from:
--   https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators

module Hydra.Util.Codetree.Script where

import Hydra.Util.Codetree.Ast
import Hydra.Util.Codetree.Print


bracketList :: Bool -> [Expr] -> Expr
bracketList newlines els = case els of
  [] -> cst "[]"
  _ -> brackets squareBrackets $ commaSep newlines els

commaOp :: Bool -> Op
commaOp newlines = Op "," (Padding WsNone (if newlines then WsBreak else WsSpace)) 0 AssociativityNone -- No source

curlyBracesList :: Bool -> [Expr] -> Expr
curlyBracesList newlines els = case els of
  [] -> cst "{}"
  _ -> brackets curlyBraces $ commaSep newlines els

ifx :: Op -> Expr -> Expr -> Expr
ifx op lhs rhs = ExprOp $ OpExpr op lhs rhs

commaSep :: Bool -> [Expr] -> Expr
commaSep newlines l = case l of
  [x] -> x
  (h:r) -> ifx (commaOp newlines) h $ commaSep newlines r

cst :: String -> Expr
cst = ExprConst

doubleNewlineSep :: [Expr] -> Expr
doubleNewlineSep = sep $ Op "" (Padding WsBreak WsBreak) 0 AssociativityNone

indentBlock :: Expr -> [Expr] -> Expr
indentBlock head els = ifx idtOp head $ newlineSep els
  where
    idtOp = Op "" (Padding WsSpace WsBreakAndIndent) 0 AssociativityNone

indentLines :: [Expr] -> Expr
indentLines els = ifx topOp (cst "") (newlineSep els)
  where
    topOp = Op "" (Padding WsNone WsBreakAndIndent) 0 AssociativityNone

newlineSep :: [Expr] -> Expr
newlineSep = sep $ Op "" (Padding WsNone WsBreak) 0 AssociativityNone

noSep :: [Expr] -> Expr
noSep = sep $ Op "" (Padding WsNone WsNone) 0 AssociativityNone

num :: Int -> Expr
num = cst . show

op :: Symbol -> Precedence -> Associativity -> Op
op s = Op s (Padding WsSpace WsSpace)

parenList :: [Expr] -> Expr
parenList els = case els of
  [] -> cst "()"
  _ -> brackets parentheses $ commaSep False els

prefix :: String -> Expr -> Expr
prefix p = ifx preOp (cst "")
  where
    preOp = Op p (Padding WsNone WsNone) 0 AssociativityNone

sep :: Op -> [Expr] -> Expr
sep op els =  case els of
  [] -> cst ""
  [x] -> x
  (h:r) -> ifx op h $ sep op r

spaceSep :: [Expr] -> Expr
spaceSep = sep $ Op "" (Padding WsSpace WsNone) 0 AssociativityNone
