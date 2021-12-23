module Hydra.Util.Codetree.Print (
  brackets,
  curlyBraces,
  indent,
  parentheses,
  parenthesize,
  printExpr,
  printExprAsTree,
  squareBrackets,
) where

import Hydra.Util.Codetree.Ast
import qualified Data.List as L


brackets :: Brackets -> Expr -> Expr
brackets br e = ExprBrackets $ BracketExpr br e

curlyBraces :: Brackets
curlyBraces = Brackets "{" "}"

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

indent :: String -> String
indent s = L.intercalate "\n" $ ("  " ++) <$> lines s

noPadding :: Padding
noPadding = Padding WsNone WsNone

printExpr :: Expr -> String
printExpr exp = case exp of
  ExprConst s -> s
  ExprOp (OpExpr (Op sym (Padding padl padr) _ _) l r) -> lhs ++ pad padl ++ sym ++ pad padr ++ rhs
    where
      lhs = idt padl $ printExpr l
      rhs = idt padr $ printExpr r
      idt ws s = if ws == WsBreakAndIndent then indent s else s
      pad ws = case ws of
        WsNone -> ""
        WsSpace -> " "
        WsBreak -> "\n"
        WsBreakAndIndent -> "\n"
  ExprBrackets (BracketExpr (Brackets l r) e) -> if L.length (lines body) > 1
      then l ++ "\n" ++ indent body ++ r
      else l ++ body ++ r
    where
      body = printExpr e

printExprAsTree :: Expr -> String
printExprAsTree expr = case expr of
  ExprConst s -> s
  ExprBrackets (BracketExpr (Brackets l r) e) -> l ++ r ++ ":\n" ++ indent (printExprAsTree e)
  ExprOp (OpExpr op l r) -> opSymbol op ++ ":\n" ++ indent (printExprAsTree l) ++ "\n" ++ indent (printExprAsTree r)

parens :: Expr -> Expr
parens = brackets parentheses

parentheses :: Brackets
parentheses = Brackets "(" ")"

squareBrackets :: Brackets
squareBrackets = Brackets "[" "]"
