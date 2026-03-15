-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ast

module Hydra.Dsl.Ast where

import qualified Hydra.Ast as Ast
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

associativityNone :: Ast.Associativity
associativityNone = Ast.AssociativityNone

associativityLeft :: Ast.Associativity
associativityLeft = Ast.AssociativityLeft

associativityRight :: Ast.Associativity
associativityRight = Ast.AssociativityRight

associativityBoth :: Ast.Associativity
associativityBoth = Ast.AssociativityBoth

blockStyle :: (Maybe String -> Bool -> Bool -> Ast.BlockStyle)
blockStyle indent newlineBeforeContent newlineAfterContent = Ast.BlockStyle {
  Ast.blockStyleIndent = indent,
  Ast.blockStyleNewlineBeforeContent = newlineBeforeContent,
  Ast.blockStyleNewlineAfterContent = newlineAfterContent}

blockStyleIndent :: (Ast.BlockStyle -> Maybe String)
blockStyleIndent = Ast.blockStyleIndent

blockStyleNewlineBeforeContent :: (Ast.BlockStyle -> Bool)
blockStyleNewlineBeforeContent = Ast.blockStyleNewlineBeforeContent

blockStyleNewlineAfterContent :: (Ast.BlockStyle -> Bool)
blockStyleNewlineAfterContent = Ast.blockStyleNewlineAfterContent

blockStyleWithIndent :: (Ast.BlockStyle -> Maybe String -> Ast.BlockStyle)
blockStyleWithIndent original newVal = Ast.BlockStyle {
  Ast.blockStyleIndent = newVal,
  Ast.blockStyleNewlineBeforeContent = (Ast.blockStyleNewlineBeforeContent original),
  Ast.blockStyleNewlineAfterContent = (Ast.blockStyleNewlineAfterContent original)}

blockStyleWithNewlineBeforeContent :: (Ast.BlockStyle -> Bool -> Ast.BlockStyle)
blockStyleWithNewlineBeforeContent original newVal = Ast.BlockStyle {
  Ast.blockStyleIndent = (Ast.blockStyleIndent original),
  Ast.blockStyleNewlineBeforeContent = newVal,
  Ast.blockStyleNewlineAfterContent = (Ast.blockStyleNewlineAfterContent original)}

blockStyleWithNewlineAfterContent :: (Ast.BlockStyle -> Bool -> Ast.BlockStyle)
blockStyleWithNewlineAfterContent original newVal = Ast.BlockStyle {
  Ast.blockStyleIndent = (Ast.blockStyleIndent original),
  Ast.blockStyleNewlineBeforeContent = (Ast.blockStyleNewlineBeforeContent original),
  Ast.blockStyleNewlineAfterContent = newVal}

bracketExpr :: (Ast.Brackets -> Ast.Expr -> Ast.BlockStyle -> Ast.BracketExpr)
bracketExpr brackets enclosed style = Ast.BracketExpr {
  Ast.bracketExprBrackets = brackets,
  Ast.bracketExprEnclosed = enclosed,
  Ast.bracketExprStyle = style}

bracketExprBrackets :: (Ast.BracketExpr -> Ast.Brackets)
bracketExprBrackets = Ast.bracketExprBrackets

bracketExprEnclosed :: (Ast.BracketExpr -> Ast.Expr)
bracketExprEnclosed = Ast.bracketExprEnclosed

bracketExprStyle :: (Ast.BracketExpr -> Ast.BlockStyle)
bracketExprStyle = Ast.bracketExprStyle

bracketExprWithBrackets :: (Ast.BracketExpr -> Ast.Brackets -> Ast.BracketExpr)
bracketExprWithBrackets original newVal = Ast.BracketExpr {
  Ast.bracketExprBrackets = newVal,
  Ast.bracketExprEnclosed = (Ast.bracketExprEnclosed original),
  Ast.bracketExprStyle = (Ast.bracketExprStyle original)}

bracketExprWithEnclosed :: (Ast.BracketExpr -> Ast.Expr -> Ast.BracketExpr)
bracketExprWithEnclosed original newVal = Ast.BracketExpr {
  Ast.bracketExprBrackets = (Ast.bracketExprBrackets original),
  Ast.bracketExprEnclosed = newVal,
  Ast.bracketExprStyle = (Ast.bracketExprStyle original)}

bracketExprWithStyle :: (Ast.BracketExpr -> Ast.BlockStyle -> Ast.BracketExpr)
bracketExprWithStyle original newVal = Ast.BracketExpr {
  Ast.bracketExprBrackets = (Ast.bracketExprBrackets original),
  Ast.bracketExprEnclosed = (Ast.bracketExprEnclosed original),
  Ast.bracketExprStyle = newVal}

brackets :: (Ast.Symbol -> Ast.Symbol -> Ast.Brackets)
brackets open close = Ast.Brackets {
  Ast.bracketsOpen = open,
  Ast.bracketsClose = close}

bracketsOpen :: (Ast.Brackets -> Ast.Symbol)
bracketsOpen = Ast.bracketsOpen

bracketsClose :: (Ast.Brackets -> Ast.Symbol)
bracketsClose = Ast.bracketsClose

bracketsWithOpen :: (Ast.Brackets -> Ast.Symbol -> Ast.Brackets)
bracketsWithOpen original newVal = Ast.Brackets {
  Ast.bracketsOpen = newVal,
  Ast.bracketsClose = (Ast.bracketsClose original)}

bracketsWithClose :: (Ast.Brackets -> Ast.Symbol -> Ast.Brackets)
bracketsWithClose original newVal = Ast.Brackets {
  Ast.bracketsOpen = (Ast.bracketsOpen original),
  Ast.bracketsClose = newVal}

exprConst :: (Ast.Symbol -> Ast.Expr)
exprConst x = (Ast.ExprConst x)

exprIndent :: (Ast.IndentedExpression -> Ast.Expr)
exprIndent x = (Ast.ExprIndent x)

exprOp :: (Ast.OpExpr -> Ast.Expr)
exprOp x = (Ast.ExprOp x)

exprBrackets :: (Ast.BracketExpr -> Ast.Expr)
exprBrackets x = (Ast.ExprBrackets x)

indentedExpression :: (Ast.IndentStyle -> Ast.Expr -> Ast.IndentedExpression)
indentedExpression style expr = Ast.IndentedExpression {
  Ast.indentedExpressionStyle = style,
  Ast.indentedExpressionExpr = expr}

indentedExpressionStyle :: (Ast.IndentedExpression -> Ast.IndentStyle)
indentedExpressionStyle = Ast.indentedExpressionStyle

indentedExpressionExpr :: (Ast.IndentedExpression -> Ast.Expr)
indentedExpressionExpr = Ast.indentedExpressionExpr

indentedExpressionWithStyle :: (Ast.IndentedExpression -> Ast.IndentStyle -> Ast.IndentedExpression)
indentedExpressionWithStyle original newVal = Ast.IndentedExpression {
  Ast.indentedExpressionStyle = newVal,
  Ast.indentedExpressionExpr = (Ast.indentedExpressionExpr original)}

indentedExpressionWithExpr :: (Ast.IndentedExpression -> Ast.Expr -> Ast.IndentedExpression)
indentedExpressionWithExpr original newVal = Ast.IndentedExpression {
  Ast.indentedExpressionStyle = (Ast.indentedExpressionStyle original),
  Ast.indentedExpressionExpr = newVal}

indentStyleAllLines :: (String -> Ast.IndentStyle)
indentStyleAllLines x = (Ast.IndentStyleAllLines x)

indentStyleSubsequentLines :: (String -> Ast.IndentStyle)
indentStyleSubsequentLines x = (Ast.IndentStyleSubsequentLines x)

op :: (Ast.Symbol -> Ast.Padding -> Ast.Precedence -> Ast.Associativity -> Ast.Op)
op symbol padding precedence associativity = Ast.Op {
  Ast.opSymbol = symbol,
  Ast.opPadding = padding,
  Ast.opPrecedence = precedence,
  Ast.opAssociativity = associativity}

opSymbol :: (Ast.Op -> Ast.Symbol)
opSymbol = Ast.opSymbol

opPadding :: (Ast.Op -> Ast.Padding)
opPadding = Ast.opPadding

opPrecedence :: (Ast.Op -> Ast.Precedence)
opPrecedence = Ast.opPrecedence

opAssociativity :: (Ast.Op -> Ast.Associativity)
opAssociativity = Ast.opAssociativity

opWithSymbol :: (Ast.Op -> Ast.Symbol -> Ast.Op)
opWithSymbol original newVal = Ast.Op {
  Ast.opSymbol = newVal,
  Ast.opPadding = (Ast.opPadding original),
  Ast.opPrecedence = (Ast.opPrecedence original),
  Ast.opAssociativity = (Ast.opAssociativity original)}

opWithPadding :: (Ast.Op -> Ast.Padding -> Ast.Op)
opWithPadding original newVal = Ast.Op {
  Ast.opSymbol = (Ast.opSymbol original),
  Ast.opPadding = newVal,
  Ast.opPrecedence = (Ast.opPrecedence original),
  Ast.opAssociativity = (Ast.opAssociativity original)}

opWithPrecedence :: (Ast.Op -> Ast.Precedence -> Ast.Op)
opWithPrecedence original newVal = Ast.Op {
  Ast.opSymbol = (Ast.opSymbol original),
  Ast.opPadding = (Ast.opPadding original),
  Ast.opPrecedence = newVal,
  Ast.opAssociativity = (Ast.opAssociativity original)}

opWithAssociativity :: (Ast.Op -> Ast.Associativity -> Ast.Op)
opWithAssociativity original newVal = Ast.Op {
  Ast.opSymbol = (Ast.opSymbol original),
  Ast.opPadding = (Ast.opPadding original),
  Ast.opPrecedence = (Ast.opPrecedence original),
  Ast.opAssociativity = newVal}

opExpr :: (Ast.Op -> Ast.Expr -> Ast.Expr -> Ast.OpExpr)
opExpr op lhs rhs = Ast.OpExpr {
  Ast.opExprOp = op,
  Ast.opExprLhs = lhs,
  Ast.opExprRhs = rhs}

opExprOp :: (Ast.OpExpr -> Ast.Op)
opExprOp = Ast.opExprOp

opExprLhs :: (Ast.OpExpr -> Ast.Expr)
opExprLhs = Ast.opExprLhs

opExprRhs :: (Ast.OpExpr -> Ast.Expr)
opExprRhs = Ast.opExprRhs

opExprWithOp :: (Ast.OpExpr -> Ast.Op -> Ast.OpExpr)
opExprWithOp original newVal = Ast.OpExpr {
  Ast.opExprOp = newVal,
  Ast.opExprLhs = (Ast.opExprLhs original),
  Ast.opExprRhs = (Ast.opExprRhs original)}

opExprWithLhs :: (Ast.OpExpr -> Ast.Expr -> Ast.OpExpr)
opExprWithLhs original newVal = Ast.OpExpr {
  Ast.opExprOp = (Ast.opExprOp original),
  Ast.opExprLhs = newVal,
  Ast.opExprRhs = (Ast.opExprRhs original)}

opExprWithRhs :: (Ast.OpExpr -> Ast.Expr -> Ast.OpExpr)
opExprWithRhs original newVal = Ast.OpExpr {
  Ast.opExprOp = (Ast.opExprOp original),
  Ast.opExprLhs = (Ast.opExprLhs original),
  Ast.opExprRhs = newVal}

padding :: (Ast.Ws -> Ast.Ws -> Ast.Padding)
padding left right = Ast.Padding {
  Ast.paddingLeft = left,
  Ast.paddingRight = right}

paddingLeft :: (Ast.Padding -> Ast.Ws)
paddingLeft = Ast.paddingLeft

paddingRight :: (Ast.Padding -> Ast.Ws)
paddingRight = Ast.paddingRight

paddingWithLeft :: (Ast.Padding -> Ast.Ws -> Ast.Padding)
paddingWithLeft original newVal = Ast.Padding {
  Ast.paddingLeft = newVal,
  Ast.paddingRight = (Ast.paddingRight original)}

paddingWithRight :: (Ast.Padding -> Ast.Ws -> Ast.Padding)
paddingWithRight original newVal = Ast.Padding {
  Ast.paddingLeft = (Ast.paddingLeft original),
  Ast.paddingRight = newVal}

precedence :: (Int -> Ast.Precedence)
precedence x = (Ast.Precedence x)

unPrecedence :: (Ast.Precedence -> Int)
unPrecedence = Ast.unPrecedence

symbol :: (String -> Ast.Symbol)
symbol x = (Ast.Symbol x)

unSymbol :: (Ast.Symbol -> String)
unSymbol = Ast.unSymbol

wsNone :: Ast.Ws
wsNone = Ast.WsNone

wsSpace :: Ast.Ws
wsSpace = Ast.WsSpace

wsBreak :: Ast.Ws
wsBreak = Ast.WsBreak

wsBreakAndIndent :: (String -> Ast.Ws)
wsBreakAndIndent x = (Ast.WsBreakAndIndent x)

wsDoubleBreak :: Ast.Ws
wsDoubleBreak = Ast.WsDoubleBreak
