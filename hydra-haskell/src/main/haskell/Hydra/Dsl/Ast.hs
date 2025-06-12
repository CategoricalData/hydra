module Hydra.Dsl.Ast where

import Hydra.Kernel
import Hydra.Dsl.Phantoms
import Hydra.Ast

import qualified Data.Map as M
import qualified Data.Int as I


-- Associativity

associativityNone :: TTerm Associativity
associativityNone = unitVariant _Associativity _Associativity_none

associativityLeft :: TTerm Associativity
associativityLeft = unitVariant _Associativity _Associativity_left

associativityRight :: TTerm Associativity
associativityRight = unitVariant _Associativity _Associativity_right

associativityBoth :: TTerm Associativity
associativityBoth = unitVariant _Associativity _Associativity_both

-- BlockStyle

blockStyle :: TTerm (Maybe String) -> TTerm Bool -> TTerm Bool -> TTerm BlockStyle
blockStyle indent newlineBefore newlineAfter = record _BlockStyle [
    _BlockStyle_indent >>: indent,
    _BlockStyle_newlineBeforeContent >>: newlineBefore,
    _BlockStyle_newlineAfterContent >>: newlineAfter]

blockStyleIndent :: TTerm BlockStyle -> TTerm (Maybe String)
blockStyleIndent bs = project _BlockStyle _BlockStyle_indent @@ bs

blockStyleNewlineBeforeContent :: TTerm BlockStyle -> TTerm Bool
blockStyleNewlineBeforeContent bs = project _BlockStyle _BlockStyle_newlineBeforeContent @@ bs

blockStyleNewlineAfterContent :: TTerm BlockStyle -> TTerm Bool
blockStyleNewlineAfterContent bs = project _BlockStyle _BlockStyle_newlineAfterContent @@ bs

-- BracketExpr

bracketExpr :: TTerm Brackets -> TTerm Expr -> TTerm BlockStyle -> TTerm BracketExpr
bracketExpr brackets enclosed style = record _BracketExpr [
    _BracketExpr_brackets >>: brackets,
    _BracketExpr_enclosed >>: enclosed,
    _BracketExpr_style >>: style]

bracketExprBrackets :: TTerm BracketExpr -> TTerm Brackets
bracketExprBrackets be = project _BracketExpr _BracketExpr_brackets @@ be

bracketExprEnclosed :: TTerm BracketExpr -> TTerm Expr
bracketExprEnclosed be = project _BracketExpr _BracketExpr_enclosed @@ be

bracketExprStyle :: TTerm BracketExpr -> TTerm BlockStyle
bracketExprStyle be = project _BracketExpr _BracketExpr_style @@ be

-- Brackets

brackets :: TTerm Symbol -> TTerm Symbol -> TTerm Brackets
brackets open close = record _Brackets [
    _Brackets_open >>: open,
    _Brackets_close >>: close]

bracketsOpen :: TTerm Brackets -> TTerm Symbol
bracketsOpen b = project _Brackets _Brackets_open @@ b

bracketsClose :: TTerm Brackets -> TTerm Symbol
bracketsClose b = project _Brackets _Brackets_close @@ b

-- Expr (sum type)

exprConst :: TTerm Symbol -> TTerm Expr
exprConst sym = variant _Expr _Expr_const sym

exprIndent :: TTerm IndentedExpression -> TTerm Expr
exprIndent ind = variant _Expr _Expr_indent ind

exprOp :: TTerm OpExpr -> TTerm Expr
exprOp op = variant _Expr _Expr_op op

exprBrackets :: TTerm BracketExpr -> TTerm Expr
exprBrackets be = variant _Expr _Expr_brackets be

-- IndentedExpression

indentedExpression :: TTerm IndentStyle -> TTerm Expr -> TTerm IndentedExpression
indentedExpression style expr = record _IndentedExpression [
    _IndentedExpression_style >>: style,
    _IndentedExpression_expr >>: expr]

indentedExpressionStyle :: TTerm IndentedExpression -> TTerm IndentStyle
indentedExpressionStyle ie = project _IndentedExpression _IndentedExpression_style @@ ie

indentedExpressionExpr :: TTerm IndentedExpression -> TTerm Expr
indentedExpressionExpr ie = project _IndentedExpression _IndentedExpression_expr @@ ie

-- IndentStyle (sum type)

indentStyleAllLines :: TTerm String -> TTerm IndentStyle
indentStyleAllLines s = variant _IndentStyle _IndentStyle_allLines s

indentStyleSubsequentLines :: TTerm String -> TTerm IndentStyle
indentStyleSubsequentLines s = variant _IndentStyle _IndentStyle_subsequentLines s

-- Op

op :: TTerm Symbol -> TTerm Padding -> TTerm Precedence -> TTerm Associativity -> TTerm Op
op symbol padding precedence associativity = record _Op [
    _Op_symbol >>: symbol,
    _Op_padding >>: padding,
    _Op_precedence >>: precedence,
    _Op_associativity >>: associativity]

opSymbol :: TTerm Op -> TTerm Symbol
opSymbol o = project _Op _Op_symbol @@ o

opPadding :: TTerm Op -> TTerm Padding
opPadding o = project _Op _Op_padding @@ o

opPrecedence :: TTerm Op -> TTerm Precedence
opPrecedence o = project _Op _Op_precedence @@ o

opAssociativity :: TTerm Op -> TTerm Associativity
opAssociativity o = project _Op _Op_associativity @@ o

-- OpExpr

opExpr :: TTerm Op -> TTerm Expr -> TTerm Expr -> TTerm OpExpr
opExpr op lhs rhs = record _OpExpr [
    _OpExpr_op >>: op,
    _OpExpr_lhs >>: lhs,
    _OpExpr_rhs >>: rhs]

opExprOp :: TTerm OpExpr -> TTerm Op
opExprOp oe = project _OpExpr _OpExpr_op @@ oe

opExprLhs :: TTerm OpExpr -> TTerm Expr
opExprLhs oe = project _OpExpr _OpExpr_lhs @@ oe

opExprRhs :: TTerm OpExpr -> TTerm Expr
opExprRhs oe = project _OpExpr _OpExpr_rhs @@ oe

-- Padding

padding :: TTerm Ws -> TTerm Ws -> TTerm Padding
padding left right = record _Padding [
    _Padding_left >>: left,
    _Padding_right >>: right]

paddingLeft :: TTerm Padding -> TTerm Ws
paddingLeft p = project _Padding _Padding_left @@ p

paddingRight :: TTerm Padding -> TTerm Ws
paddingRight p = project _Padding _Padding_right @@ p

-- Precedence (newtype wrapping Int)

precedence :: TTerm I.Int -> TTerm Precedence
precedence = wrap _Precedence

unPrecedence :: TTerm Precedence -> TTerm I.Int
unPrecedence p = unwrap _Precedence @@ p

-- Symbol (newtype wrapping String)

symbol :: TTerm String -> TTerm Symbol
symbol = wrap _Symbol

unSymbol :: TTerm Symbol -> TTerm String
unSymbol s = unwrap _Symbol @@ s

-- Ws (sum type)

wsNone :: TTerm Ws
wsNone = unitVariant _Ws _Ws_none

wsSpace :: TTerm Ws
wsSpace = unitVariant _Ws _Ws_space

wsBreak :: TTerm Ws
wsBreak = unitVariant _Ws _Ws_break

wsBreakAndIndent :: TTerm String -> TTerm Ws
wsBreakAndIndent s = variant _Ws _Ws_breakAndIndent s

wsDoubleBreak :: TTerm Ws
wsDoubleBreak = unitVariant _Ws _Ws_doubleBreak
