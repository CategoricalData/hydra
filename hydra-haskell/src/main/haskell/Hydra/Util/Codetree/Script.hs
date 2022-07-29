module Hydra.Util.Codetree.Script where

import Hydra.Util.Codetree.Ast

import qualified Data.List as L


angleBraces :: Brackets
angleBraces = Brackets (sym "<") (sym ">")

angleBracesList ::  BlockStyle -> [Expr] -> Expr
angleBracesList style els = case els of
  [] -> cst "<>"
  _ -> brackets angleBraces style $ commaSep style els

bracketList :: BlockStyle -> [Expr] -> Expr
bracketList style els = case els of
  [] -> cst "[]"
  _ -> brackets squareBrackets style $ commaSep style els

brackets :: Brackets -> BlockStyle -> Expr -> Expr
brackets br style e = ExprBrackets $ BracketExpr br e style

commaOp :: Bool -> Op
commaOp newlines = Op (sym ",") (Padding WsNone (if newlines then WsBreak else WsSpace)) (Precedence 0) AssociativityNone -- No source

commaSep :: BlockStyle -> [Expr] -> Expr
commaSep style l = case l of
  [] -> cst ""
  [x] -> x
  (h:r) -> ifx (commaOp newlines) h $ commaSep style r
  where
    newlines = blockStyleNewlineBeforeContent style

curlyBlock :: BlockStyle -> Expr -> Expr
curlyBlock style e = curlyBracesList style [e]

curlyBraces :: Brackets
curlyBraces = Brackets (sym "{") (sym "}")

curlyBracesList :: BlockStyle -> [Expr] -> Expr
curlyBracesList style els = case els of
  [] -> cst "{}"
  _ -> brackets curlyBraces style $ commaSep style els

cst :: String -> Expr
cst = ExprConst . Symbol

dotSep :: [Expr] -> Expr
dotSep = sep $ Op (sym ".") (Padding WsNone WsNone) (Precedence 0) AssociativityNone

doubleNewlineSep :: [Expr] -> Expr
doubleNewlineSep = sep $ Op (sym "") (Padding WsBreak WsBreak) (Precedence 0) AssociativityNone

fullBlockStyle :: BlockStyle
fullBlockStyle = BlockStyle True True True

halfBlockStyle :: BlockStyle
halfBlockStyle = BlockStyle True True False

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

infixWs :: String -> Expr -> Expr -> Expr
infixWs op l r = spaceSep [l, cst op, r]

infixWsList :: String -> [Expr] -> Expr
infixWsList op opers = spaceSep $ L.foldl (\e r -> if L.null e then [r] else r:opExpr:e) [] $ L.reverse opers
  where
    opExpr = cst op

inlineStyle :: BlockStyle
inlineStyle = BlockStyle False False False

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
  _ -> brackets parentheses inlineStyle $ commaSep inlineStyle els

parens :: Expr -> Expr
parens = brackets parentheses inlineStyle

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
  ExprBrackets (BracketExpr br e newlines) -> ExprBrackets (BracketExpr br (parenthesize e) newlines)
  _ -> exp

prefix :: String -> Expr -> Expr
prefix p = ifx preOp (cst "")
  where
    preOp = Op (sym p) (Padding WsNone WsNone) (Precedence 0) AssociativityNone

printExpr :: Expr -> String
printExpr e = case e of
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
  ExprBrackets (BracketExpr (Brackets (Symbol l) (Symbol r)) e style) ->
      l ++ pre ++ ibody ++ suf ++ r
    where
      body = printExpr e
      ibody = if doIndent then indent body else body
      pre = if nlBefore then "\n" else ""
      suf = if nlAfter then "\n" else ""
      BlockStyle doIndent nlBefore nlAfter = style

printExprAsTree :: Expr -> String
printExprAsTree expr = case expr of
  ExprConst (Symbol s) -> s
  ExprBrackets (BracketExpr (Brackets (Symbol l) (Symbol r)) e _) -> l ++ r ++ ":\n" ++ indent (printExprAsTree e)
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
