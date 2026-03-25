-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ast

module Hydra.Dsl.Ast where

import qualified Hydra.Ast as Ast
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

associativityBoth :: Phantoms.TTerm Ast.Associativity
associativityBoth =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "both"),
        Core.fieldTerm = Core.TermUnit}}))

associativityLeft :: Phantoms.TTerm Ast.Associativity
associativityLeft =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "left"),
        Core.fieldTerm = Core.TermUnit}}))

associativityNone :: Phantoms.TTerm Ast.Associativity
associativityNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

associativityRight :: Phantoms.TTerm Ast.Associativity
associativityRight =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Associativity"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "right"),
        Core.fieldTerm = Core.TermUnit}}))

blockStyle :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyle indent newlineBeforeContent newlineAfterContent =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Phantoms.unTTerm indent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Phantoms.unTTerm newlineBeforeContent)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Phantoms.unTTerm newlineAfterContent)}]}))

blockStyleIndent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm (Maybe String)
blockStyleIndent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionField = (Core.Name "indent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockStyleNewlineAfterContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool
blockStyleNewlineAfterContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionField = (Core.Name "newlineAfterContent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockStyleNewlineBeforeContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool
blockStyleNewlineBeforeContent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
        Core.projectionField = (Core.Name "newlineBeforeContent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

blockStyleWithIndent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithIndent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "newlineBeforeContent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "newlineAfterContent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

blockStyleWithNewlineAfterContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithNewlineAfterContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "indent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "newlineBeforeContent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

blockStyleWithNewlineBeforeContent :: Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Bool -> Phantoms.TTerm Ast.BlockStyle
blockStyleWithNewlineBeforeContent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BlockStyle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "indent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "indent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newlineBeforeContent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newlineAfterContent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BlockStyle"),
              Core.projectionField = (Core.Name "newlineAfterContent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketExpr :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Ast.BracketExpr
bracketExpr brackets enclosed style =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Phantoms.unTTerm brackets)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Phantoms.unTTerm enclosed)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)}]}))

bracketExprBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Brackets
bracketExprBrackets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionField = (Core.Name "brackets")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketExprEnclosed :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr
bracketExprEnclosed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionField = (Core.Name "enclosed")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketExprStyle :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.BlockStyle
bracketExprStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
        Core.projectionField = (Core.Name "style")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketExprWithBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithBrackets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "enclosed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "style")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketExprWithEnclosed :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithEnclosed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "brackets")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "style")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bracketExprWithStyle :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.BlockStyle -> Phantoms.TTerm Ast.BracketExpr
bracketExprWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.BracketExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "brackets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "brackets")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enclosed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.BracketExpr"),
              Core.projectionField = (Core.Name "enclosed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

brackets :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
brackets open close =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm open)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Phantoms.unTTerm close)}]}))

bracketsClose :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol
bracketsClose x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionField = (Core.Name "close")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketsOpen :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol
bracketsOpen x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
        Core.projectionField = (Core.Name "open")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bracketsWithClose :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
bracketsWithClose original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionField = (Core.Name "open")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

bracketsWithOpen :: Phantoms.TTerm Ast.Brackets -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Brackets
bracketsWithOpen original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Brackets"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "open"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "close"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Brackets"),
              Core.projectionField = (Core.Name "close")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exprBrackets :: Phantoms.TTerm Ast.BracketExpr -> Phantoms.TTerm Ast.Expr
exprBrackets x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "brackets"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exprConst :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Expr
exprConst x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exprIndent :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr
exprIndent x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "indent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exprOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
exprOp x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "op"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exprSeq :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Expr
exprSeq x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Expr"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "seq"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

indentStyleAllLines :: Phantoms.TTerm String -> Phantoms.TTerm Ast.IndentStyle
indentStyleAllLines x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "allLines"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

indentStyleSubsequentLines :: Phantoms.TTerm String -> Phantoms.TTerm Ast.IndentStyle
indentStyleSubsequentLines x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.IndentStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subsequentLines"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

indentedExpression :: Phantoms.TTerm Ast.IndentStyle -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.IndentedExpression
indentedExpression style expr =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm style)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm expr)}]}))

indentedExpressionExpr :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr
indentedExpressionExpr x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionField = (Core.Name "expr")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indentedExpressionStyle :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.IndentStyle
indentedExpressionStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
        Core.projectionField = (Core.Name "style")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indentedExpressionWithExpr :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.IndentedExpression
indentedExpressionWithExpr original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionField = (Core.Name "style")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

indentedExpressionWithStyle :: Phantoms.TTerm Ast.IndentedExpression -> Phantoms.TTerm Ast.IndentStyle -> Phantoms.TTerm Ast.IndentedExpression
indentedExpressionWithStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.IndentedExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "style"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expr"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.IndentedExpression"),
              Core.projectionField = (Core.Name "expr")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

op :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Ast.Associativity -> Phantoms.TTerm Ast.Op
op symbol padding precedence associativity =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm symbol)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Phantoms.unTTerm padding)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Phantoms.unTTerm precedence)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm associativity)}]}))

opAssociativity :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Associativity
opAssociativity x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionField = (Core.Name "associativity")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opExpr :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExpr op lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

opExprLhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
opExprLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionField = (Core.Name "lhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opExprOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Op
opExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opExprRhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr
opExprRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
        Core.projectionField = (Core.Name "rhs")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opExprWithLhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExprWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

opExprWithOp :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.OpExpr
opExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "rhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

opExprWithRhs :: Phantoms.TTerm Ast.OpExpr -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Ast.OpExpr
opExprWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.OpExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.OpExpr"),
              Core.projectionField = (Core.Name "lhs")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

opPadding :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Padding
opPadding x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionField = (Core.Name "padding")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opPrecedence :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Precedence
opPrecedence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionField = (Core.Name "precedence")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opSymbol :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Symbol
opSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
        Core.projectionField = (Core.Name "symbol")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

opWithAssociativity :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Associativity -> Phantoms.TTerm Ast.Op
opWithAssociativity original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "symbol")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "padding")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "precedence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

opWithPadding :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Op
opWithPadding original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "symbol")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "precedence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "associativity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

opWithPrecedence :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Ast.Op
opWithPrecedence original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "symbol")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "padding")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "associativity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

opWithSymbol :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm Ast.Op
opWithSymbol original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Op"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "symbol"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "padding"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "padding")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "precedence"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "precedence")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "associativity"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Op"),
              Core.projectionField = (Core.Name "associativity")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

padding :: Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
padding left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

paddingLeft :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws
paddingLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paddingRight :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws
paddingRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

paddingWithLeft :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
paddingWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

paddingWithRight :: Phantoms.TTerm Ast.Padding -> Phantoms.TTerm Ast.Ws -> Phantoms.TTerm Ast.Padding
paddingWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.Padding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.Padding"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

precedence :: Phantoms.TTerm Int -> Phantoms.TTerm Ast.Precedence
precedence x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Precedence"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

seqExpr :: Phantoms.TTerm Ast.Op -> Phantoms.TTerm [Ast.Expr] -> Phantoms.TTerm Ast.SeqExpr
seqExpr op elements =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm op)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm elements)}]}))

seqExprElements :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm [Ast.Expr]
seqExprElements x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionField = (Core.Name "elements")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

seqExprOp :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Op
seqExprOp x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
        Core.projectionField = (Core.Name "op")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

seqExprWithElements :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm [Ast.Expr] -> Phantoms.TTerm Ast.SeqExpr
seqExprWithElements original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionField = (Core.Name "op")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

seqExprWithOp :: Phantoms.TTerm Ast.SeqExpr -> Phantoms.TTerm Ast.Op -> Phantoms.TTerm Ast.SeqExpr
seqExprWithOp original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.ast.SeqExpr"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "op"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "elements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.ast.SeqExpr"),
              Core.projectionField = (Core.Name "elements")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

symbol :: Phantoms.TTerm String -> Phantoms.TTerm Ast.Symbol
symbol x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.ast.Symbol"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPrecedence :: Phantoms.TTerm Ast.Precedence -> Phantoms.TTerm Int
unPrecedence x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ast.Precedence")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSymbol :: Phantoms.TTerm Ast.Symbol -> Phantoms.TTerm String
unSymbol x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ast.Symbol")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

wsBreak :: Phantoms.TTerm Ast.Ws
wsBreak =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = Core.TermUnit}}))

wsBreakAndIndent :: Phantoms.TTerm String -> Phantoms.TTerm Ast.Ws
wsBreakAndIndent x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "breakAndIndent"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

wsDoubleBreak :: Phantoms.TTerm Ast.Ws
wsDoubleBreak =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doubleBreak"),
        Core.fieldTerm = Core.TermUnit}}))

wsNone :: Phantoms.TTerm Ast.Ws
wsNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

wsSpace :: Phantoms.TTerm Ast.Ws
wsSpace =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.ast.Ws"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "space"),
        Core.fieldTerm = Core.TermUnit}}))
