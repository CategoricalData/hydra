-- | Utilities for constructing generic program code ASTs, used for the serialization phase of source code generation.

module Hydra.Staging.Serialization where

import Hydra.Ast

import qualified Data.List as L
import qualified Data.Maybe as Y


angleBraces :: Brackets
angleBraces = Brackets (sym "<") (sym ">")

angleBracesList ::  BlockStyle -> [Expr] -> Expr
angleBracesList style els = case els of
  [] -> cst "<>"
  _ -> brackets angleBraces style $ commaSep style els

-- | Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width
--   of the expression.
bracesListAdaptive :: [Expr] -> Expr
bracesListAdaptive els = if (expressionLength inlineList) > 70
    then curlyBracesList Nothing halfBlockStyle els
    else inlineList
  where
    inlineList = curlyBracesList Nothing inlineStyle els
    maxLength = L.maximum (expressionLength <$> els)

bracketList :: BlockStyle -> [Expr] -> Expr
bracketList style els = case els of
  [] -> cst "[]"
  _ -> brackets squareBrackets style $ commaSep style els

-- | Produce a bracketed list which separates elements by spaces or newlines depending on the estimated width
--   of the expression.
bracketListAdaptive :: [Expr] -> Expr
bracketListAdaptive els = if (expressionLength inlineList) > 70
    then bracketList halfBlockStyle els
    else inlineList
  where
    inlineList = bracketList inlineStyle els
    maxLength = L.maximum (expressionLength <$> els)

brackets :: Brackets -> BlockStyle -> Expr -> Expr
brackets br style e = ExprBrackets $ BracketExpr br e style

commaSep :: BlockStyle -> [Expr] -> Expr
commaSep = symbolSep ","

curlyBlock :: BlockStyle -> Expr -> Expr
curlyBlock style e = curlyBracesList Nothing style [e]

curlyBraces :: Brackets
curlyBraces = Brackets (sym "{") (sym "}")

curlyBracesList :: Maybe String -> BlockStyle -> [Expr] -> Expr
curlyBracesList msymb style els = case els of
  [] -> cst "{}"
  _ -> brackets curlyBraces style $ symbolSep (Y.fromMaybe "," msymb) style els

cst :: String -> Expr
cst = ExprConst . Symbol

customIndent :: String -> String -> String
customIndent idt s = L.intercalate "\n" $ (idt ++) <$> lines s

customIndentBlock :: String -> [Expr] -> Expr
customIndentBlock idt els = case els of
  [] -> cst ""
  [x] -> x
  (head:rest) -> ifx idtOp head $ newlineSep rest
    where
      idtOp = Op (sym "") (Padding WsSpace $ WsBreakAndIndent idt) (Precedence 0) AssociativityNone

dotSep :: [Expr] -> Expr
dotSep = sep $ Op (sym ".") (Padding WsNone WsNone) (Precedence 0) AssociativityNone

doubleNewlineSep :: [Expr] -> Expr
doubleNewlineSep = sep $ Op (sym "") (Padding WsBreak WsBreak) (Precedence 0) AssociativityNone

doubleSpace :: String
doubleSpace = "  "

-- | Find the approximate length (number of characters, including spaces and newlines) of an expression
--   without actually printing it.
expressionLength :: Expr -> Int
expressionLength e = case e of
    ExprConst s -> symbolLength s
    ExprIndent ie -> indentedExpressionLength ie
    ExprOp oe -> opExprLength oe
    ExprBrackets be -> bracketExprLength be
  where
    blockStyleLength (BlockStyle mindent nlBefore nlAfter)
      = (Y.maybe 0 L.length mindent) + (if nlBefore then 1 else 0) + (if nlAfter then 1 else 0)
    bracketExprLength (BracketExpr brackets e style)
      = bracketsLength brackets + expressionLength e + blockStyleLength style
    bracketsLength (Brackets open close) = symbolLength open + symbolLength close
    indentedExpressionLength (IndentedExpression style e) = expressionLength e + case style of
      IndentStyleAllLines s -> L.length s
      IndentStyleSubsequentLines s -> L.length s
    opExprLength (OpExpr op el er) = opLength op + expressionLength el + expressionLength er
    opLength (Op s (Padding pl pr) _ _) = symbolLength s + wsLength pl + wsLength pr
    symbolLength (Symbol s) = L.length s
    wsLength ws = case ws of
      WsNone -> 0
      WsSpace -> 1
      WsBreak -> 1
      WsBreakAndIndent s -> 1 + L.length s
      WsDoubleBreak -> 2

fullBlockStyle :: BlockStyle
fullBlockStyle = BlockStyle (Just doubleSpace) True True

halfBlockStyle :: BlockStyle
halfBlockStyle = BlockStyle (Just doubleSpace) True False

ifx :: Op -> Expr -> Expr -> Expr
ifx op lhs rhs = ExprOp $ OpExpr op lhs rhs

indent :: String -> String
indent = customIndent doubleSpace

indentBlock :: [Expr] -> Expr
indentBlock = customIndentBlock doubleSpace

indentLines :: Bool -> [Expr] -> Expr
indentLines doubleSp els = ifx topOp (cst "") (if doubleSp then doubleNewlineSep els else newlineSep els)
  where
    topOp = Op (sym "") (Padding WsNone (WsBreakAndIndent doubleSpace)) (Precedence 0) AssociativityNone

indentSubsequentLines :: String -> Expr -> Expr
indentSubsequentLines idt e = ExprIndent $ IndentedExpression (IndentStyleSubsequentLines idt) e

infixWs :: String -> Expr -> Expr -> Expr
infixWs op l r = spaceSep [l, cst op, r]

infixWsList :: String -> [Expr] -> Expr
infixWsList op opers = spaceSep $ L.foldl (\e r -> if L.null e then [r] else r:opExpr:e) [] $ L.reverse opers
  where
    opExpr = cst op

inlineStyle :: BlockStyle
inlineStyle = BlockStyle Nothing False False

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

orOp :: Bool -> Op
orOp newlines = Op (sym "|") (Padding WsSpace (if newlines then WsBreak else WsSpace)) (Precedence 0) AssociativityNone -- No source

orSep :: BlockStyle -> [Expr] -> Expr
orSep style l = case l of
  [] -> cst ""
  [x] -> x
  (h:r) -> ifx (orOp newlines) h $ orSep style r
  where
    newlines = blockStyleNewlineBeforeContent style

parenList :: Bool -> [Expr] -> Expr
parenList newlines els = case els of
    [] -> cst "()"
    _ -> brackets parentheses style $ commaSep style els
  where
    style = if newlines && L.length els > 1 then halfBlockStyle else inlineStyle

parens :: Expr -> Expr
parens = brackets parentheses inlineStyle

parentheses :: Brackets
parentheses = Brackets (sym "(") (sym ")")

parenthesize :: Expr -> Expr
parenthesize exp = case exp of
  ExprBrackets (BracketExpr br e newlines) -> ExprBrackets (BracketExpr br (parenthesize e) newlines)
  ExprConst _ -> exp
  ExprIndent (IndentedExpression style e) -> ExprIndent (IndentedExpression style (parenthesize e))
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

prefix :: String -> Expr -> Expr
prefix p = ifx preOp (cst "")
  where
    preOp = Op (sym p) (Padding WsNone WsNone) (Precedence 0) AssociativityNone

printExpr :: Expr -> String
printExpr e = case e of
  ExprConst (Symbol s) -> s
  ExprIndent (IndentedExpression style expr) -> L.intercalate "\n" $ case style of
      IndentStyleAllLines idt -> (idt ++) <$> lns
      IndentStyleSubsequentLines idt -> case lns of
        [x] -> [x]
        (head:rest) -> head:((idt ++) <$> rest)
    where
      lns = lines $ printExpr expr
  ExprOp (OpExpr (Op (Symbol sym) (Padding padl padr) _ _) l r) -> lhs ++ pad padl ++ sym ++ pad padr ++ rhs
    where
      lhs = idt padl $ printExpr l
      rhs = idt padr $ printExpr r
      idt ws s = case ws of
        WsBreakAndIndent idt -> customIndent idt s
        _ -> s
      pad ws = case ws of
        WsNone -> ""
        WsSpace -> " "
        WsBreak -> "\n"
        WsBreakAndIndent _ -> "\n"
        WsDoubleBreak -> "\n\n"
  ExprBrackets (BracketExpr (Brackets (Symbol l) (Symbol r)) e style) ->
      l ++ pre ++ ibody ++ suf ++ r
    where
      body = printExpr e
      ibody = case doIndent of
         Nothing -> body
         Just idt -> customIndent idt body
      pre = if nlBefore then "\n" else ""
      suf = if nlAfter then "\n" else ""
      BlockStyle doIndent nlBefore nlAfter = style

semicolonSep :: [Expr] -> Expr
semicolonSep = symbolSep ";" inlineStyle

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

symbolSep :: String -> BlockStyle -> [Expr] -> Expr
symbolSep symb style l = case l of
    [] -> cst ""
    [x] -> x
    (h:r) -> ifx commaOp h $ symbolSep symb style r
  where
    break = case L.length $ L.filter id [blockStyleNewlineBeforeContent style, blockStyleNewlineAfterContent style] of
      0 -> WsSpace
      1 -> WsBreak
      2 -> WsDoubleBreak
    commaOp = Op (sym symb) (Padding WsNone break) (Precedence 0) AssociativityNone -- No source

tabIndent :: Expr -> Expr
tabIndent e = ExprIndent $ IndentedExpression (IndentStyleAllLines "    ") e

tabIndentDoubleSpace :: [Expr] -> Expr
tabIndentDoubleSpace = tabIndent . doubleNewlineSep

tabIndentSingleSpace :: [Expr] -> Expr
tabIndentSingleSpace = tabIndent . newlineSep

unsupportedType :: String -> Expr
unsupportedType label = cst $ "[" ++ label ++ "]"

unsupportedVariant :: Show a => String -> a -> Expr
unsupportedVariant label obj = cst $ "[unsupported " ++ label ++ ": " ++ show obj ++ "]"

withComma :: Expr -> Expr
withComma e = noSep [e, cst ","]

withSemi :: Expr -> Expr
withSemi e = noSep [e, cst ";"]
