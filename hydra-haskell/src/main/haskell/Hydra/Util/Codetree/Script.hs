module Hydra.Util.Codetree.Script where

import Hydra.Util.Codetree.Ast

import qualified Data.List as L


angleBraces :: Brackets
angleBraces = Brackets (sym "<") (sym ">")

angleBracesList :: Bool -> [Expr] -> Expr
angleBracesList newlines els = case els of
  [] -> cst "<>"
  _ -> brackets angleBraces $ commaSep newlines els

bracketList :: Bool -> [Expr] -> Expr
bracketList newlines els = case els of
  [] -> cst "[]"
  _ -> brackets squareBrackets $ commaSep newlines els

brackets :: Brackets -> Expr -> Expr
brackets br e = ExprBrackets $ BracketExpr br e

commaOp :: Bool -> Op
commaOp newlines = Op (sym ",") (Padding WsNone (if newlines then WsBreak else WsSpace)) (Precedence 0) AssociativityNone -- No source

commaSep :: Bool -> [Expr] -> Expr
commaSep newlines l = case l of
  [x] -> x
  (h:r) -> ifx (commaOp newlines) h $ commaSep newlines r

curlyBlock :: Expr -> Expr
curlyBlock e = curlyBracesList True [e]

curlyBraces :: Brackets
curlyBraces = Brackets (sym "{") (sym "}")

curlyBracesList :: Bool -> [Expr] -> Expr
curlyBracesList newlines els = case els of
  [] -> cst "{}"
  _ -> brackets curlyBraces $ commaSep newlines els

cst :: String -> Expr
cst = ExprConst . Symbol

dotSep :: [Expr] -> Expr
dotSep = sep $ Op (sym ".") (Padding WsNone WsNone) (Precedence 0) AssociativityNone

doubleNewlineSep :: [Expr] -> Expr
doubleNewlineSep = sep $ Op (sym "") (Padding WsBreak WsBreak) (Precedence 0) AssociativityNone

ifx :: Op -> Expr -> Expr -> Expr
ifx op lhs rhs = ExprOp $ OpExpr op lhs rhs

indent :: String -> String
indent s = L.intercalate "\n" $ ("  " ++) <$> lines s

indentBlock :: Expr -> [Expr] -> Expr
indentBlock head els = ifx idtOp head $ newlineSep els
  where
    idtOp = Op (sym "") (Padding WsSpace WsBreakAndIndent) (Precedence 0) AssociativityNone

indentLines :: [Expr] -> Expr
indentLines els = ifx topOp (cst "") (newlineSep els)
  where
    topOp = Op (sym "") (Padding WsNone WsBreakAndIndent) (Precedence 0) AssociativityNone

newlineSep :: [Expr] -> Expr
newlineSep = sep $ Op (sym "") (Padding WsNone WsBreak) (Precedence 0) AssociativityNone

noPadding :: Padding
noPadding = Padding WsNone WsNone

noSep :: [Expr] -> Expr
noSep = sep $ Op (sym "") (Padding WsNone WsNone) (Precedence 0) AssociativityNone

num :: Int -> Expr
num = cst . show

op :: String -> Int -> Associativity -> Op
op s p = Op (Symbol s) (Padding WsSpace WsSpace) (Precedence p)

parenList :: [Expr] -> Expr
parenList els = case els of
  [] -> cst "()"
  _ -> brackets parentheses $ commaSep False els

parens :: Expr -> Expr
parens = brackets parentheses

parentheses :: Brackets
parentheses = Brackets (sym "(") (sym ")")

parenthesize :: Expr -> Expr
parenthesize exp = case exp of
  ExprOp (OpExpr op@(Op _ _ prec assoc) lhs rhs) -> ExprOp (OpExpr op lhs2 rhs2)
    where
      lhs' = parenthesize lhs
      rhs' = parenthesize rhs
      lhs2 = case lhs' of
        ExprOp (OpExpr (Op _ _ lprec lassoc) _ _) -> case prec `compare` lprec of
          LT -> lhs'
          GT -> parens lhs'
          EQ -> if assocLeft assoc && assocLeft lassoc
            then lhs'
            else parens lhs'
        _ -> lhs'
      rhs2 = case rhs' of
        ExprOp (OpExpr (Op _ _ rprec rassoc) _ _) -> case prec `compare` rprec of
          LT -> rhs'
          GT -> parens rhs'
          EQ -> if assocRight assoc && assocRight rassoc
            then rhs'
            else parens rhs'
        _ -> rhs'
      assocLeft a = a == AssociativityLeft || a == AssociativityNone || a == AssociativityBoth
      assocRight a = a == AssociativityRight || a == AssociativityNone || a == AssociativityBoth
  ExprBrackets (BracketExpr br e) -> ExprBrackets (BracketExpr br $ parenthesize e)
  _ -> exp

prefix :: String -> Expr -> Expr
prefix p = ifx preOp (cst "")
  where
    preOp = Op (sym p) (Padding WsNone WsNone) (Precedence 0) AssociativityNone

printExpr :: Expr -> String
printExpr exp = case exp of
  ExprConst (Symbol s) -> s
  ExprOp (OpExpr (Op (Symbol sym) (Padding padl padr) _ _) l r) -> lhs ++ pad padl ++ sym ++ pad padr ++ rhs
    where
      lhs = idt padl $ printExpr l
      rhs = idt padr $ printExpr r
      idt ws s = if ws == WsBreakAndIndent then indent s else s
      pad ws = case ws of
        WsNone -> ""
        WsSpace -> " "
        WsBreak -> "\n"
        WsBreakAndIndent -> "\n"
  ExprBrackets (BracketExpr (Brackets (Symbol l) (Symbol r)) e) -> if L.length (lines body) > 1
      then l ++ "\n" ++ indent body ++ r
      else l ++ body ++ r
    where
      body = printExpr e

printExprAsTree :: Expr -> String
printExprAsTree expr = case expr of
  ExprConst (Symbol s) -> s
  ExprBrackets (BracketExpr (Brackets (Symbol l) (Symbol r)) e) -> l ++ r ++ ":\n" ++ indent (printExprAsTree e)
  ExprOp (OpExpr op l r) -> h (opSymbol op) ++ ":\n" ++ indent (printExprAsTree l) ++ "\n" ++ indent (printExprAsTree r)
    where
      h (Symbol s) = s

sep :: Op -> [Expr] -> Expr
sep op els =  case els of
  [] -> cst ""
  [x] -> x
  (h:r) -> ifx op h $ sep op r

spaceSep :: [Expr] -> Expr
spaceSep = sep $ Op (sym "") (Padding WsSpace WsNone) (Precedence 0) AssociativityNone

squareBrackets :: Brackets
squareBrackets = Brackets (sym "[") (sym "]")

sym :: String -> Symbol
sym = Symbol
