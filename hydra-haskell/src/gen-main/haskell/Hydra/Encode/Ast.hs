-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.ast

module Hydra.Encode.Ast where

import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Maybes as Maybes
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

associativity :: (Ast.Associativity -> Core.Term)
associativity x = case x of
  Ast.AssociativityNone -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "none"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.AssociativityLeft -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.AssociativityRight -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.AssociativityBoth -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "both"),
      Core.fieldTerm = Core.TermUnit}}))

blockStyle :: (Ast.BlockStyle -> Core.Term)
blockStyle x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "indent"),
      Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x -> Core.TermLiteral (Core.LiteralString x)) opt)) (Ast.blockStyleIndent x))},
    Core.Field {
      Core.fieldName = (Core.Name "newlineBeforeContent"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralBoolean x)) (Ast.blockStyleNewlineBeforeContent x))},
    Core.Field {
      Core.fieldName = (Core.Name "newlineAfterContent"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralBoolean x)) (Ast.blockStyleNewlineAfterContent x))}]}))

bracketExpr :: (Ast.BracketExpr -> Core.Term)
bracketExpr x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "brackets"),
      Core.fieldTerm = (brackets (Ast.bracketExprBrackets x))},
    Core.Field {
      Core.fieldName = (Core.Name "enclosed"),
      Core.fieldTerm = (expr (Ast.bracketExprEnclosed x))},
    Core.Field {
      Core.fieldName = (Core.Name "style"),
      Core.fieldTerm = (blockStyle (Ast.bracketExprStyle x))}]}))

brackets :: (Ast.Brackets -> Core.Term)
brackets x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "open"),
      Core.fieldTerm = (symbol (Ast.bracketsOpen x))},
    Core.Field {
      Core.fieldName = (Core.Name "close"),
      Core.fieldTerm = (symbol (Ast.bracketsClose x))}]}))

expr :: (Ast.Expr -> Core.Term)
expr x = case x of
  Ast.ExprConst v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "const"),
      Core.fieldTerm = (symbol v1)}}))
  Ast.ExprIndent v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "indent"),
      Core.fieldTerm = (indentedExpression v1)}}))
  Ast.ExprOp v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (opExpr v1)}}))
  Ast.ExprBrackets v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "brackets"),
      Core.fieldTerm = (bracketExpr v1)}}))

indentedExpression :: (Ast.IndentedExpression -> Core.Term)
indentedExpression x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "style"),
      Core.fieldTerm = (indentStyle (Ast.indentedExpressionStyle x))},
    Core.Field {
      Core.fieldName = (Core.Name "expr"),
      Core.fieldTerm = (expr (Ast.indentedExpressionExpr x))}]}))

indentStyle :: (Ast.IndentStyle -> Core.Term)
indentStyle x = case x of
  Ast.IndentStyleAllLines v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "allLines"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))
  Ast.IndentStyleSubsequentLines v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "subsequentLines"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))

op :: (Ast.Op -> Core.Term)
op x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.Op"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "symbol"),
      Core.fieldTerm = (symbol (Ast.opSymbol x))},
    Core.Field {
      Core.fieldName = (Core.Name "padding"),
      Core.fieldTerm = (padding (Ast.opPadding x))},
    Core.Field {
      Core.fieldName = (Core.Name "precedence"),
      Core.fieldTerm = (precedence (Ast.opPrecedence x))},
    Core.Field {
      Core.fieldName = (Core.Name "associativity"),
      Core.fieldTerm = (associativity (Ast.opAssociativity x))}]}))

opExpr :: (Ast.OpExpr -> Core.Term)
opExpr x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "op"),
      Core.fieldTerm = (op (Ast.opExprOp x))},
    Core.Field {
      Core.fieldName = (Core.Name "lhs"),
      Core.fieldTerm = (expr (Ast.opExprLhs x))},
    Core.Field {
      Core.fieldName = (Core.Name "rhs"),
      Core.fieldTerm = (expr (Ast.opExprRhs x))}]}))

padding :: (Ast.Padding -> Core.Term)
padding x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (ws (Ast.paddingLeft x))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (ws (Ast.paddingRight x))}]}))

precedence :: (Ast.Precedence -> Core.Term)
precedence x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ast.Precedence"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 x))) (Ast.unPrecedence x))}))

symbol :: (Ast.Symbol -> Core.Term)
symbol x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ast.Symbol"),
  Core.wrappedTermBody = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Ast.unSymbol x))}))

ws :: (Ast.Ws -> Core.Term)
ws x = case x of
  Ast.WsNone -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "none"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.WsSpace -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "space"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.WsBreak -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "break"),
      Core.fieldTerm = Core.TermUnit}}))
  Ast.WsBreakAndIndent v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "breakAndIndent"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString v1))}}))
  Ast.WsDoubleBreak -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "doubleBreak"),
      Core.fieldTerm = Core.TermUnit}}))
