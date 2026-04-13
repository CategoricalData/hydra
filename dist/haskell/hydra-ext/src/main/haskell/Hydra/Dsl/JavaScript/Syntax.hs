-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.javaScript.syntax

module Hydra.Dsl.JavaScript.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.JavaScript.Syntax as Syntax
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Int as I

arrayElementExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrayElement
arrayElementExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayElementHole :: Phantoms.TTerm Syntax.ArrayElement
arrayElementHole =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hole"),
        Core.fieldTerm = Core.TermUnit}}))

arrayElementSpread :: Phantoms.TTerm Syntax.SpreadElement -> Phantoms.TTerm Syntax.ArrayElement
arrayElementSpread x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "spread"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrayTypeExpression :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.ArrayTypeExpression
arrayTypeExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.ArrayTypeExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

arrowFunctionBodyBlock :: Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.ArrowFunctionBody
arrowFunctionBodyBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrowFunctionBodyExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ArrowFunctionBody
arrowFunctionBodyExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

arrowFunctionExpression :: Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ArrowFunctionBody -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ArrowFunctionExpression
arrowFunctionExpression params body async =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)}]}))

arrowFunctionExpressionAsync :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm Bool
arrowFunctionExpressionAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
        Core.projectionField = (Core.Name "async")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrowFunctionExpressionBody :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm Syntax.ArrowFunctionBody
arrowFunctionExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrowFunctionExpressionParams :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm [Syntax.Pattern]
arrowFunctionExpressionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

arrowFunctionExpressionWithAsync :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

arrowFunctionExpressionWithBody :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm Syntax.ArrowFunctionBody -> Phantoms.TTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

arrowFunctionExpressionWithParams :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ArrowFunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentExpression :: Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpression operator left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

assignmentExpressionLeft :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Pattern
assignmentExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentExpressionOperator :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.AssignmentOperator
assignmentExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentExpressionRight :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
assignmentExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentExpressionWithLeft :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentExpressionWithOperator :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.AssignmentOperator -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentExpressionWithRight :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentExpression
assignmentExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

assignmentOperatorAddAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAddAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorAndAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAndAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "andAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseAndAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseAndAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAndAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseOrAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseOrAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOrAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorBitwiseXorAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseXorAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXorAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorDivideAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorDivideAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divideAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorExponentiateAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorExponentiateAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exponentiateAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorLeftShiftAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorLeftShiftAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorModuloAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorModuloAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "moduloAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorMultiplyAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorMultiplyAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiplyAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorNullishAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorNullishAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullishAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorOrAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorOrAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorRightShiftAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorRightShiftAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorSubtractAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorSubtractAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtractAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentOperatorUnsignedRightShiftAssign :: Phantoms.TTerm Syntax.AssignmentOperator
assignmentOperatorUnsignedRightShiftAssign =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedRightShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))

assignmentPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentPattern
assignmentPattern left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

assignmentPatternLeft :: Phantoms.TTerm Syntax.AssignmentPattern -> Phantoms.TTerm Syntax.Pattern
assignmentPatternLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentPatternRight :: Phantoms.TTerm Syntax.AssignmentPattern -> Phantoms.TTerm Syntax.Expression
assignmentPatternRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

assignmentPatternWithLeft :: Phantoms.TTerm Syntax.AssignmentPattern -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.AssignmentPattern
assignmentPatternWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

assignmentPatternWithRight :: Phantoms.TTerm Syntax.AssignmentPattern -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.AssignmentPattern
assignmentPatternWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.AssignmentPattern"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryExpression :: Phantoms.TTerm Syntax.BinaryOperator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpression
binaryExpression operator left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

binaryExpressionLeft :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.Expression
binaryExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionOperator :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.BinaryOperator
binaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionRight :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.Expression
binaryExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionWithLeft :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpression
binaryExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithOperator :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.BinaryOperator -> Phantoms.TTerm Syntax.BinaryExpression
binaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithRight :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.BinaryExpression
binaryExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryOperatorAdd :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorAdd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorAnd :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseAnd :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseOr :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBitwiseXor :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorBitwiseXor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorDivide :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorDivide =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorExponentiate :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorExponentiate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exponentiate"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreaterThan :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorGreaterThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreaterThanOrEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorGreaterThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorIn :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorIn =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorInstanceof :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorInstanceof =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceof"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLeftShift :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLeftShift =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLessThan :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLessThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLessThanOrEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorLessThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorModulo :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorModulo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorMultiply :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorMultiply =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorNotEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorNullishCoalescing :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorNullishCoalescing =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullishCoalescing"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorOr :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorRightShift :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorRightShift =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorStrictEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorStrictEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorStrictNotEqual :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorStrictNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictNotEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorSubtract :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorSubtract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorUnsignedRightShift :: Phantoms.TTerm Syntax.BinaryOperator
binaryOperatorUnsignedRightShift =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedRightShift"),
        Core.fieldTerm = Core.TermUnit}}))

callExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.CallExpression
callExpression callee arguments optional =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Phantoms.unTTerm callee)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm optional)}]}))

callExpressionArguments :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm [Syntax.Expression]
callExpressionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

callExpressionCallee :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Syntax.Expression
callExpressionCallee x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
        Core.projectionField = (Core.Name "callee")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

callExpressionOptional :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Bool
callExpressionOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
        Core.projectionField = (Core.Name "optional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

callExpressionWithArguments :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.CallExpression
callExpressionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "callee")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

callExpressionWithCallee :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.CallExpression
callExpressionWithCallee original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

callExpressionWithOptional :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.CallExpression
callExpressionWithOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "callee")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CallExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

catchClause :: Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.CatchClause
catchClause param body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

catchClauseBody :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.BlockStatement
catchClauseBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchClauseParam :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.Pattern)
catchClauseParam x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
        Core.projectionField = (Core.Name "param")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

catchClauseWithBody :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
              Core.projectionField = (Core.Name "param")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

catchClauseWithParam :: Phantoms.TTerm Syntax.CatchClause -> Phantoms.TTerm (Maybe Syntax.Pattern) -> Phantoms.TTerm Syntax.CatchClause
catchClauseWithParam original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.CatchClause"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDeclaration :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclaration id superClass body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Phantoms.unTTerm superClass)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

classDeclarationBody :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassBody
classDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationId :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.Identifier
classDeclarationId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationSuperClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm (Maybe Syntax.Expression)
classDeclarationSuperClass x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
        Core.projectionField = (Core.Name "superClass")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationWithBody :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassBody -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "superClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classDeclarationWithComments :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.ClassDeclarationWithComments
classDeclarationWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

classDeclarationWithCommentsBody :: Phantoms.TTerm Syntax.ClassDeclarationWithComments -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.ClassDeclarationWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment)
classDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

classDeclarationWithCommentsWithBody :: Phantoms.TTerm Syntax.ClassDeclarationWithComments -> Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.ClassDeclarationWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclarationWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

classDeclarationWithId :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "superClass")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

classDeclarationWithSuperClass :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ClassDeclaration
classDeclarationWithSuperClass original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ClassDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

commentBlock :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
commentBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commentDocumentation :: Phantoms.TTerm Syntax.DocumentationComment -> Phantoms.TTerm Syntax.Comment
commentDocumentation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "documentation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commentLine :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Comment
commentLine x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

conditionalExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpression test consequent alternate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm consequent)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Phantoms.unTTerm alternate)}]}))

conditionalExpressionAlternate :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression
conditionalExpressionAlternate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
        Core.projectionField = (Core.Name "alternate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpressionConsequent :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression
conditionalExpressionConsequent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
        Core.projectionField = (Core.Name "consequent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpressionTest :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression
conditionalExpressionTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionalExpressionWithAlternate :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionWithAlternate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "consequent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

conditionalExpressionWithConsequent :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionWithConsequent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "alternate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionalExpressionWithTest :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ConditionalExpression
conditionalExpressionWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "consequent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ConditionalExpression"),
              Core.projectionField = (Core.Name "alternate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doWhileStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoWhileStatement
doWhileStatement body test =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)}]}))

doWhileStatementBody :: Phantoms.TTerm Syntax.DoWhileStatement -> Phantoms.TTerm Syntax.Statement
doWhileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doWhileStatementTest :: Phantoms.TTerm Syntax.DoWhileStatement -> Phantoms.TTerm Syntax.Expression
doWhileStatementTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

doWhileStatementWithBody :: Phantoms.TTerm Syntax.DoWhileStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.DoWhileStatement
doWhileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

doWhileStatementWithTest :: Phantoms.TTerm Syntax.DoWhileStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.DoWhileStatement
doWhileStatementWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DoWhileStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

documentationComment :: Phantoms.TTerm String -> Phantoms.TTerm [Syntax.DocumentationTag] -> Phantoms.TTerm Syntax.DocumentationComment
documentationComment description tags =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm tags)}]}))

documentationCommentDescription :: Phantoms.TTerm Syntax.DocumentationComment -> Phantoms.TTerm String
documentationCommentDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationCommentTags :: Phantoms.TTerm Syntax.DocumentationComment -> Phantoms.TTerm [Syntax.DocumentationTag]
documentationCommentTags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
        Core.projectionField = (Core.Name "tags")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationCommentWithDescription :: Phantoms.TTerm Syntax.DocumentationComment -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DocumentationComment
documentationCommentWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
              Core.projectionField = (Core.Name "tags")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

documentationCommentWithTags :: Phantoms.TTerm Syntax.DocumentationComment -> Phantoms.TTerm [Syntax.DocumentationTag] -> Phantoms.TTerm Syntax.DocumentationComment
documentationCommentWithTags original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationComment"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

documentationTag :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DocumentationTag
documentationTag name type_ paramName description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Phantoms.unTTerm paramName)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))

documentationTagDescription :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm String
documentationTagDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationTagName :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm String
documentationTagName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationTagParamName :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm (Maybe Syntax.Identifier)
documentationTagParamName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
        Core.projectionField = (Core.Name "paramName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationTagType :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm (Maybe Syntax.TypeExpression)
documentationTagType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

documentationTagWithDescription :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DocumentationTag
documentationTagWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "paramName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

documentationTagWithName :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.DocumentationTag
documentationTagWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "paramName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

documentationTagWithParamName :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.DocumentationTag
documentationTagWithParamName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

documentationTagWithType :: Phantoms.TTerm Syntax.DocumentationTag -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.DocumentationTag
documentationTagWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "paramName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.DocumentationTag"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exportAllDeclaration :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.ExportAllDeclaration
exportAllDeclaration exported source =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Phantoms.unTTerm exported)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)}]}))

exportAllDeclarationExported :: Phantoms.TTerm Syntax.ExportAllDeclaration -> Phantoms.TTerm (Maybe Syntax.Identifier)
exportAllDeclarationExported x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
        Core.projectionField = (Core.Name "exported")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportAllDeclarationSource :: Phantoms.TTerm Syntax.ExportAllDeclaration -> Phantoms.TTerm Syntax.StringLiteral
exportAllDeclarationSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportAllDeclarationWithExported :: Phantoms.TTerm Syntax.ExportAllDeclaration -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.ExportAllDeclaration
exportAllDeclarationWithExported original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

exportAllDeclarationWithSource :: Phantoms.TTerm Syntax.ExportAllDeclaration -> Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.ExportAllDeclaration
exportAllDeclarationWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportAllDeclaration"),
              Core.projectionField = (Core.Name "exported")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exportDeclarationAll :: Phantoms.TTerm Syntax.ExportAllDeclaration -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationAll x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDeclarationDeclaration :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDeclarationDefault :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationDefault x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportDeclarationNamed :: Phantoms.TTerm Syntax.NamedExport -> Phantoms.TTerm Syntax.ExportDeclaration
exportDeclarationNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

exportSpecifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExportSpecifier
exportSpecifier local exported =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm local)},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Phantoms.unTTerm exported)}]}))

exportSpecifierExported :: Phantoms.TTerm Syntax.ExportSpecifier -> Phantoms.TTerm Syntax.Identifier
exportSpecifierExported x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
        Core.projectionField = (Core.Name "exported")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportSpecifierLocal :: Phantoms.TTerm Syntax.ExportSpecifier -> Phantoms.TTerm Syntax.Identifier
exportSpecifierLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
        Core.projectionField = (Core.Name "local")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

exportSpecifierWithExported :: Phantoms.TTerm Syntax.ExportSpecifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExportSpecifier
exportSpecifierWithExported original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
              Core.projectionField = (Core.Name "local")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

exportSpecifierWithLocal :: Phantoms.TTerm Syntax.ExportSpecifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ExportSpecifier
exportSpecifierWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ExportSpecifier"),
              Core.projectionField = (Core.Name "exported")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

expressionArray :: Phantoms.TTerm Syntax.ArrayExpression -> Phantoms.TTerm Syntax.Expression
expressionArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionArrow :: Phantoms.TTerm Syntax.ArrowFunctionExpression -> Phantoms.TTerm Syntax.Expression
expressionArrow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrow"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAssignment :: Phantoms.TTerm Syntax.AssignmentExpression -> Phantoms.TTerm Syntax.Expression
expressionAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAwait :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionAwait x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "await"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBinary :: Phantoms.TTerm Syntax.BinaryExpression -> Phantoms.TTerm Syntax.Expression
expressionBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionCall :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Syntax.Expression
expressionCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionConditional :: Phantoms.TTerm Syntax.ConditionalExpression -> Phantoms.TTerm Syntax.Expression
expressionConditional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionFunction :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Syntax.Expression
expressionFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Expression
expressionIdentifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionMember :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Syntax.Expression
expressionMember x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionNew :: Phantoms.TTerm Syntax.CallExpression -> Phantoms.TTerm Syntax.Expression
expressionNew x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionObject :: Phantoms.TTerm Syntax.ObjectExpression -> Phantoms.TTerm Syntax.Expression
expressionObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionParenthesized :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression
expressionParenthesized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionSequence :: Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.Expression
expressionSequence x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionSpread :: Phantoms.TTerm Syntax.SpreadElement -> Phantoms.TTerm Syntax.Expression
expressionSpread x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "spread"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionThis :: Phantoms.TTerm Syntax.Expression
expressionThis =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))

expressionUnary :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.Expression
expressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionYield :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Expression
expressionYield x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInLeftPattern :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.ForInLeft
forInLeftPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInLeft"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInLeftVariable :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.ForInLeft
forInLeftVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInLeft"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInStatement :: Phantoms.TTerm Syntax.ForInLeft -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForInStatement
forInStatement left right body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

forInStatementBody :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.Statement
forInStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forInStatementLeft :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.ForInLeft
forInStatementLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forInStatementRight :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.Expression
forInStatementRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forInStatementWithBody :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForInStatement
forInStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forInStatementWithLeft :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.ForInLeft -> Phantoms.TTerm Syntax.ForInStatement
forInStatementWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forInStatementWithRight :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForInStatement
forInStatementWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forInitExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForInit
forInitExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forInitVariable :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.ForInit
forInitVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

forOfStatement :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForInLeft -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForOfStatement
forOfStatement await left right body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Phantoms.unTTerm await)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

forOfStatementAwait :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Bool
forOfStatementAwait x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
        Core.projectionField = (Core.Name "await")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forOfStatementBody :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.Statement
forOfStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forOfStatementLeft :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.ForInLeft
forOfStatementLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forOfStatementRight :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.Expression
forOfStatementRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forOfStatementWithAwait :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.ForOfStatement
forOfStatementWithAwait original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forOfStatementWithBody :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForOfStatement
forOfStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "await")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forOfStatementWithLeft :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.ForInLeft -> Phantoms.TTerm Syntax.ForOfStatement
forOfStatementWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "await")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forOfStatementWithRight :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ForOfStatement
forOfStatementWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "await")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForOfStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forStatement :: Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForStatement
forStatement init test update body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Phantoms.unTTerm update)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

forStatementBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement
forStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementInit :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.ForInit)
forStatementInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementTest :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
forStatementTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementUpdate :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Expression)
forStatementUpdate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
        Core.projectionField = (Core.Name "update")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

forStatementWithBody :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ForStatement
forStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "update")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

forStatementWithInit :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.ForInit) -> Phantoms.TTerm Syntax.ForStatement
forStatementWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "update")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forStatementWithTest :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForStatement
forStatementWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "update")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

forStatementWithUpdate :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.ForStatement
forStatementWithUpdate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ForStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclaration :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclaration id params body async generator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Phantoms.unTTerm generator)}]}))

functionDeclarationAsync :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Bool
functionDeclarationAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "async")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationBody :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.BlockStatement
functionDeclarationBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationGenerator :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Bool
functionDeclarationGenerator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "generator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationId :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.Identifier
functionDeclarationId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationParams :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.Pattern]
functionDeclarationParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationWithAsync :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithBody :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithComments :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

functionDeclarationWithCommentsBody :: Phantoms.TTerm Syntax.FunctionDeclarationWithComments -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationWithCommentsComments :: Phantoms.TTerm Syntax.FunctionDeclarationWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment)
functionDeclarationWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionDeclarationWithCommentsWithBody :: Phantoms.TTerm Syntax.FunctionDeclarationWithComments -> Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithCommentsWithComments :: Phantoms.TTerm Syntax.FunctionDeclarationWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclarationWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionDeclarationWithGenerator :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithGenerator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionDeclarationWithId :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionDeclarationWithParams :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.FunctionDeclaration
functionDeclarationWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionDeclaration"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionExpression :: Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionExpression
functionExpression id params body async generator =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Phantoms.unTTerm generator)}]}))

functionExpressionAsync :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Bool
functionExpressionAsync x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
        Core.projectionField = (Core.Name "async")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionBody :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Syntax.BlockStatement
functionExpressionBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionGenerator :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Bool
functionExpressionGenerator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
        Core.projectionField = (Core.Name "generator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionId :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm (Maybe Syntax.Identifier)
functionExpressionId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionParams :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm [Syntax.Pattern]
functionExpressionParams x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
        Core.projectionField = (Core.Name "params")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionWithAsync :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionExpression
functionExpressionWithAsync original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionExpressionWithBody :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.FunctionExpression
functionExpressionWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionExpressionWithGenerator :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.FunctionExpression
functionExpressionWithGenerator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionExpressionWithId :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm (Maybe Syntax.Identifier) -> Phantoms.TTerm Syntax.FunctionExpression
functionExpressionWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "params")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionExpressionWithParams :: Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm [Syntax.Pattern] -> Phantoms.TTerm Syntax.FunctionExpression
functionExpressionWithParams original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "async")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionExpression"),
              Core.projectionField = (Core.Name "generator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionTypeExpression :: Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.FunctionTypeExpression
functionTypeExpression typeParameters parameters returnType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Phantoms.unTTerm typeParameters)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm returnType)}]}))

functionTypeExpressionParameters :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm [Syntax.TypeExpression]
functionTypeExpressionParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
        Core.projectionField = (Core.Name "parameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeExpressionReturnType :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
functionTypeExpressionReturnType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
        Core.projectionField = (Core.Name "returnType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeExpressionTypeParameters :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm [Syntax.TypeParameter]
functionTypeExpressionTypeParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
        Core.projectionField = (Core.Name "typeParameters")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionTypeExpressionWithParameters :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "typeParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionTypeExpressionWithReturnType :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithReturnType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "typeParameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionTypeExpressionWithTypeParameters :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm [Syntax.TypeParameter] -> Phantoms.TTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithTypeParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "parameters")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.FunctionTypeExpression"),
              Core.projectionField = (Core.Name "returnType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

identifier :: Phantoms.TTerm String -> Phantoms.TTerm Syntax.Identifier
identifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.Identifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

ifStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm (Maybe Syntax.Statement) -> Phantoms.TTerm Syntax.IfStatement
ifStatement test consequent alternate =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm consequent)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Phantoms.unTTerm alternate)}]}))

ifStatementAlternate :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.Statement)
ifStatementAlternate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
        Core.projectionField = (Core.Name "alternate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifStatementConsequent :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Statement
ifStatementConsequent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
        Core.projectionField = (Core.Name "consequent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifStatementTest :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Expression
ifStatementTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

ifStatementWithAlternate :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm (Maybe Syntax.Statement) -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithAlternate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "consequent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

ifStatementWithConsequent :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithConsequent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "alternate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

ifStatementWithTest :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.IfStatement
ifStatementWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "consequent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.IfStatement"),
              Core.projectionField = (Core.Name "alternate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importClauseDefault :: Phantoms.TTerm Syntax.ImportDefaultSpecifier -> Phantoms.TTerm Syntax.ImportClause
importClauseDefault x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importClauseNamed :: Phantoms.TTerm Syntax.ImportSpecifier -> Phantoms.TTerm Syntax.ImportClause
importClauseNamed x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importClauseNamespace :: Phantoms.TTerm Syntax.ImportNamespaceSpecifier -> Phantoms.TTerm Syntax.ImportClause
importClauseNamespace x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "namespace"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

importDeclaration :: Phantoms.TTerm [Syntax.ImportClause] -> Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclaration specifiers source =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Phantoms.unTTerm specifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)}]}))

importDeclarationSource :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.StringLiteral
importDeclarationSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDeclarationSpecifiers :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm [Syntax.ImportClause]
importDeclarationSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
        Core.projectionField = (Core.Name "specifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importDeclarationWithSource :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
              Core.projectionField = (Core.Name "specifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

importDeclarationWithSpecifiers :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm [Syntax.ImportClause] -> Phantoms.TTerm Syntax.ImportDeclaration
importDeclarationWithSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportDeclaration"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importDefaultSpecifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportDefaultSpecifier
importDefaultSpecifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.ImportDefaultSpecifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

importNamespaceSpecifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportNamespaceSpecifier
importNamespaceSpecifier x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.ImportNamespaceSpecifier"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

importSpecifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportSpecifier
importSpecifier imported local =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Phantoms.unTTerm imported)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm local)}]}))

importSpecifierImported :: Phantoms.TTerm Syntax.ImportSpecifier -> Phantoms.TTerm Syntax.Identifier
importSpecifierImported x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
        Core.projectionField = (Core.Name "imported")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importSpecifierLocal :: Phantoms.TTerm Syntax.ImportSpecifier -> Phantoms.TTerm Syntax.Identifier
importSpecifierLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
        Core.projectionField = (Core.Name "local")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

importSpecifierWithImported :: Phantoms.TTerm Syntax.ImportSpecifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportSpecifier
importSpecifierWithImported original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
              Core.projectionField = (Core.Name "local")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

importSpecifierWithLocal :: Phantoms.TTerm Syntax.ImportSpecifier -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.ImportSpecifier
importSpecifierWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ImportSpecifier"),
              Core.projectionField = (Core.Name "imported")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

labeledStatement :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatement label body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

labeledStatementBody :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
labeledStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementLabel :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier
labeledStatementLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labeledStatementWithBody :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

labeledStatementWithLabel :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.LabeledStatement
labeledStatementWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.LabeledStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

literalBigInt :: Phantoms.TTerm Integer -> Phantoms.TTerm Syntax.Literal
literalBigInt x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigInt"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalNull :: Phantoms.TTerm Syntax.Literal
literalNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

literalNumber :: Phantoms.TTerm Syntax.NumericLiteral -> Phantoms.TTerm Syntax.Literal
literalNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Syntax.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalTemplate :: Phantoms.TTerm Syntax.TemplateLiteral -> Phantoms.TTerm Syntax.Literal
literalTemplate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalUndefined :: Phantoms.TTerm Syntax.Literal
literalUndefined =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefined"),
        Core.fieldTerm = Core.TermUnit}}))

memberExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MemberExpression
memberExpression object property computed optional =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm optional)}]}))

memberExpressionComputed :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Bool
memberExpressionComputed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
        Core.projectionField = (Core.Name "computed")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberExpressionObject :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Syntax.Expression
memberExpressionObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
        Core.projectionField = (Core.Name "object")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberExpressionOptional :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Bool
memberExpressionOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
        Core.projectionField = (Core.Name "optional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberExpressionProperty :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Syntax.Expression
memberExpressionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

memberExpressionWithComputed :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MemberExpression
memberExpressionWithComputed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memberExpressionWithObject :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MemberExpression
memberExpressionWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

memberExpressionWithOptional :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MemberExpression
memberExpressionWithOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

memberExpressionWithProperty :: Phantoms.TTerm Syntax.MemberExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MemberExpression
memberExpressionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MemberExpression"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDefinition :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Syntax.MethodKind -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinition key value kind computed static =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Phantoms.unTTerm static)}]}))

methodDefinitionComputed :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Bool
methodDefinitionComputed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
        Core.projectionField = (Core.Name "computed")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDefinitionKey :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.Expression
methodDefinitionKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDefinitionKind :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.MethodKind
methodDefinitionKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
        Core.projectionField = (Core.Name "kind")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDefinitionStatic :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Bool
methodDefinitionStatic x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
        Core.projectionField = (Core.Name "static")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDefinitionValue :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.FunctionExpression
methodDefinitionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

methodDefinitionWithComputed :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinitionWithComputed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "static")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDefinitionWithKey :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinitionWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "static")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDefinitionWithKind :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.MethodKind -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinitionWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "static")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodDefinitionWithStatic :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinitionWithStatic original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

methodDefinitionWithValue :: Phantoms.TTerm Syntax.MethodDefinition -> Phantoms.TTerm Syntax.FunctionExpression -> Phantoms.TTerm Syntax.MethodDefinition
methodDefinitionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodDefinition"),
              Core.projectionField = (Core.Name "static")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

methodKindConstructor :: Phantoms.TTerm Syntax.MethodKind
methodKindConstructor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = Core.TermUnit}}))

methodKindGet :: Phantoms.TTerm Syntax.MethodKind
methodKindGet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "get"),
        Core.fieldTerm = Core.TermUnit}}))

methodKindMethod :: Phantoms.TTerm Syntax.MethodKind
methodKindMethod =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = Core.TermUnit}}))

methodKindSet :: Phantoms.TTerm Syntax.MethodKind
methodKindSet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

moduleItemExport :: Phantoms.TTerm Syntax.ExportDeclaration -> Phantoms.TTerm Syntax.ModuleItem
moduleItemExport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleItemImport :: Phantoms.TTerm Syntax.ImportDeclaration -> Phantoms.TTerm Syntax.ModuleItem
moduleItemImport x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleItemStatement :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.ModuleItem
moduleItemStatement x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

moduleItemWithComments :: Phantoms.TTerm Syntax.ModuleItem -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.ModuleItemWithComments
moduleItemWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

moduleItemWithCommentsBody :: Phantoms.TTerm Syntax.ModuleItemWithComments -> Phantoms.TTerm Syntax.ModuleItem
moduleItemWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleItemWithCommentsComments :: Phantoms.TTerm Syntax.ModuleItemWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment)
moduleItemWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleItemWithCommentsWithBody :: Phantoms.TTerm Syntax.ModuleItemWithComments -> Phantoms.TTerm Syntax.ModuleItem -> Phantoms.TTerm Syntax.ModuleItemWithComments
moduleItemWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleItemWithCommentsWithComments :: Phantoms.TTerm Syntax.ModuleItemWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.ModuleItemWithComments
moduleItemWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ModuleItemWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namedExport :: Phantoms.TTerm [Syntax.ExportSpecifier] -> Phantoms.TTerm (Maybe Syntax.StringLiteral) -> Phantoms.TTerm Syntax.NamedExport
namedExport specifiers source =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Phantoms.unTTerm specifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)}]}))

namedExportSource :: Phantoms.TTerm Syntax.NamedExport -> Phantoms.TTerm (Maybe Syntax.StringLiteral)
namedExportSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedExportSpecifiers :: Phantoms.TTerm Syntax.NamedExport -> Phantoms.TTerm [Syntax.ExportSpecifier]
namedExportSpecifiers x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
        Core.projectionField = (Core.Name "specifiers")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namedExportWithSource :: Phantoms.TTerm Syntax.NamedExport -> Phantoms.TTerm (Maybe Syntax.StringLiteral) -> Phantoms.TTerm Syntax.NamedExport
namedExportWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
              Core.projectionField = (Core.Name "specifiers")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

namedExportWithSpecifiers :: Phantoms.TTerm Syntax.NamedExport -> Phantoms.TTerm [Syntax.ExportSpecifier] -> Phantoms.TTerm Syntax.NamedExport
namedExportWithSpecifiers original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.NamedExport"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

numericLiteralFloat :: Phantoms.TTerm Double -> Phantoms.TTerm Syntax.NumericLiteral
numericLiteralFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numericLiteralInteger :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Syntax.NumericLiteral
numericLiteralInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPattern :: Phantoms.TTerm [Syntax.ObjectPatternProperty] -> Phantoms.TTerm Syntax.ObjectPattern
objectPattern properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ObjectPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

objectPatternProperties :: Phantoms.TTerm Syntax.ObjectPattern -> Phantoms.TTerm [Syntax.ObjectPatternProperty]
objectPatternProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ObjectPattern"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

objectPatternPropertyProperty :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.ObjectPatternProperty
objectPatternPropertyProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ObjectPatternProperty"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPatternPropertyRest :: Phantoms.TTerm Syntax.RestElement -> Phantoms.TTerm Syntax.ObjectPatternProperty
objectPatternPropertyRest x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.ObjectPatternProperty"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

objectPatternWithProperties :: Phantoms.TTerm Syntax.ObjectPattern -> Phantoms.TTerm [Syntax.ObjectPatternProperty] -> Phantoms.TTerm Syntax.ObjectPattern
objectPatternWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ObjectPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parameterizedTypeExpression :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpression base arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTTerm base)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

parameterizedTypeExpressionArguments :: Phantoms.TTerm Syntax.ParameterizedTypeExpression -> Phantoms.TTerm [Syntax.TypeExpression]
parameterizedTypeExpressionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
        Core.projectionField = (Core.Name "arguments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterizedTypeExpressionBase :: Phantoms.TTerm Syntax.ParameterizedTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
parameterizedTypeExpressionBase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
        Core.projectionField = (Core.Name "base")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterizedTypeExpressionWithArguments :: Phantoms.TTerm Syntax.ParameterizedTypeExpression -> Phantoms.TTerm [Syntax.TypeExpression] -> Phantoms.TTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
              Core.projectionField = (Core.Name "base")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parameterizedTypeExpressionWithBase :: Phantoms.TTerm Syntax.ParameterizedTypeExpression -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithBase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.ParameterizedTypeExpression"),
              Core.projectionField = (Core.Name "arguments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternArray :: Phantoms.TTerm Syntax.ArrayPattern -> Phantoms.TTerm Syntax.Pattern
patternArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternAssignment :: Phantoms.TTerm Syntax.AssignmentPattern -> Phantoms.TTerm Syntax.Pattern
patternAssignment x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.Pattern
patternIdentifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternObject :: Phantoms.TTerm Syntax.ObjectPattern -> Phantoms.TTerm Syntax.Pattern
patternObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternRest :: Phantoms.TTerm Syntax.RestElement -> Phantoms.TTerm Syntax.Pattern
patternRest x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

program :: Phantoms.TTerm [Syntax.ModuleItem] -> Phantoms.TTerm Syntax.SourceType -> Phantoms.TTerm Syntax.Program
program body sourceType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Phantoms.unTTerm sourceType)}]}))

programBody :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ModuleItem]
programBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programSourceType :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.SourceType
programSourceType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
        Core.projectionField = (Core.Name "sourceType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

programWithBody :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm [Syntax.ModuleItem] -> Phantoms.TTerm Syntax.Program
programWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
              Core.projectionField = (Core.Name "sourceType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

programWithSourceType :: Phantoms.TTerm Syntax.Program -> Phantoms.TTerm Syntax.SourceType -> Phantoms.TTerm Syntax.Program
programWithSourceType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Program"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

property :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.PropertyKind -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Property
property key value kind computed shorthand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Phantoms.unTTerm shorthand)}]}))

propertyComputed :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Bool
propertyComputed x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
        Core.projectionField = (Core.Name "computed")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyKey :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.Expression
propertyKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyKind :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.PropertyKind
propertyKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
        Core.projectionField = (Core.Name "kind")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyKindGet :: Phantoms.TTerm Syntax.PropertyKind
propertyKindGet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "get"),
        Core.fieldTerm = Core.TermUnit}}))

propertyKindInit :: Phantoms.TTerm Syntax.PropertyKind
propertyKindInit =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = Core.TermUnit}}))

propertyKindSet :: Phantoms.TTerm Syntax.PropertyKind
propertyKindSet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))

propertyShorthand :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Bool
propertyShorthand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
        Core.projectionField = (Core.Name "shorthand")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySignature :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.PropertySignature
propertySignature name type_ optional readonly =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm optional)},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Phantoms.unTTerm readonly)}]}))

propertySignatureName :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Syntax.Identifier
propertySignatureName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySignatureOptional :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Bool
propertySignatureOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
        Core.projectionField = (Core.Name "optional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySignatureReadonly :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Bool
propertySignatureReadonly x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
        Core.projectionField = (Core.Name "readonly")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySignatureType :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Syntax.TypeExpression
propertySignatureType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertySignatureWithName :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.PropertySignature
propertySignatureWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "readonly")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertySignatureWithOptional :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.PropertySignature
propertySignatureWithOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "readonly")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertySignatureWithReadonly :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.PropertySignature
propertySignatureWithReadonly original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertySignatureWithType :: Phantoms.TTerm Syntax.PropertySignature -> Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.PropertySignature
propertySignatureWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.PropertySignature"),
              Core.projectionField = (Core.Name "readonly")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyValue :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.Expression
propertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyWithComputed :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Property
propertyWithComputed original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "shorthand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithKey :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Property
propertyWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "shorthand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithKind :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.PropertyKind -> Phantoms.TTerm Syntax.Property
propertyWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "shorthand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyWithShorthand :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.Property
propertyWithShorthand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyWithValue :: Phantoms.TTerm Syntax.Property -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Property
propertyWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "computed")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.Property"),
              Core.projectionField = (Core.Name "shorthand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

restElement :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.RestElement
restElement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.RestElement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

sourceTypeModule :: Phantoms.TTerm Syntax.SourceType
sourceTypeModule =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.SourceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = Core.TermUnit}}))

sourceTypeScript :: Phantoms.TTerm Syntax.SourceType
sourceTypeScript =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.SourceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "script"),
        Core.fieldTerm = Core.TermUnit}}))

spreadElement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SpreadElement
spreadElement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.SpreadElement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

statementBlock :: Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.Statement
statementBlock x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementBreak :: Phantoms.TTerm Syntax.BreakStatement -> Phantoms.TTerm Syntax.Statement
statementBreak x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementClassDeclaration :: Phantoms.TTerm Syntax.ClassDeclaration -> Phantoms.TTerm Syntax.Statement
statementClassDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementContinue :: Phantoms.TTerm Syntax.ContinueStatement -> Phantoms.TTerm Syntax.Statement
statementContinue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementDebugger :: Phantoms.TTerm Syntax.Statement
statementDebugger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "debugger"),
        Core.fieldTerm = Core.TermUnit}}))

statementDoWhile :: Phantoms.TTerm Syntax.DoWhileStatement -> Phantoms.TTerm Syntax.Statement
statementDoWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doWhile"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementEmpty :: Phantoms.TTerm Syntax.Statement
statementEmpty =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))

statementExpression :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement
statementExpression x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementFor :: Phantoms.TTerm Syntax.ForStatement -> Phantoms.TTerm Syntax.Statement
statementFor x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementForIn :: Phantoms.TTerm Syntax.ForInStatement -> Phantoms.TTerm Syntax.Statement
statementForIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forIn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementForOf :: Phantoms.TTerm Syntax.ForOfStatement -> Phantoms.TTerm Syntax.Statement
statementForOf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forOf"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementFunctionDeclaration :: Phantoms.TTerm Syntax.FunctionDeclaration -> Phantoms.TTerm Syntax.Statement
statementFunctionDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementIf :: Phantoms.TTerm Syntax.IfStatement -> Phantoms.TTerm Syntax.Statement
statementIf x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementLabeled :: Phantoms.TTerm Syntax.LabeledStatement -> Phantoms.TTerm Syntax.Statement
statementLabeled x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementReturn :: Phantoms.TTerm Syntax.ReturnStatement -> Phantoms.TTerm Syntax.Statement
statementReturn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementSwitch :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Statement
statementSwitch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementThrow :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.Statement
statementThrow x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementTry :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.Statement
statementTry x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementVariableDeclaration :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.Statement
statementVariableDeclaration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableDeclaration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWhile :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
statementWhile x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

statementWithComments :: Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.StatementWithComments
statementWithComments body comments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)}]}))

statementWithCommentsBody :: Phantoms.TTerm Syntax.StatementWithComments -> Phantoms.TTerm Syntax.Statement
statementWithCommentsBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statementWithCommentsComments :: Phantoms.TTerm Syntax.StatementWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment)
statementWithCommentsComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
        Core.projectionField = (Core.Name "comments")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

statementWithCommentsWithBody :: Phantoms.TTerm Syntax.StatementWithComments -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.StatementWithComments
statementWithCommentsWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
              Core.projectionField = (Core.Name "comments")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

statementWithCommentsWithComments :: Phantoms.TTerm Syntax.StatementWithComments -> Phantoms.TTerm (Maybe Syntax.DocumentationComment) -> Phantoms.TTerm Syntax.StatementWithComments
statementWithCommentsWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StatementWithComments"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StringLiteral
stringLiteral value singleQuote =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Phantoms.unTTerm singleQuote)}]}))

stringLiteralSingleQuote :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Bool
stringLiteralSingleQuote x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
        Core.projectionField = (Core.Name "singleQuote")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralValue :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm String
stringLiteralValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringLiteralWithSingleQuote :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.StringLiteral
stringLiteralWithSingleQuote original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringLiteralWithValue :: Phantoms.TTerm Syntax.StringLiteral -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.StringLiteral
stringLiteralWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.StringLiteral"),
              Core.projectionField = (Core.Name "singleQuote")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

switchCase :: Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.SwitchCase
switchCase test consequent =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm consequent)}]}))

switchCaseConsequent :: Phantoms.TTerm Syntax.SwitchCase -> Phantoms.TTerm [Syntax.Statement]
switchCaseConsequent x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
        Core.projectionField = (Core.Name "consequent")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchCaseTest :: Phantoms.TTerm Syntax.SwitchCase -> Phantoms.TTerm (Maybe Syntax.Expression)
switchCaseTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchCaseWithConsequent :: Phantoms.TTerm Syntax.SwitchCase -> Phantoms.TTerm [Syntax.Statement] -> Phantoms.TTerm Syntax.SwitchCase
switchCaseWithConsequent original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchCaseWithTest :: Phantoms.TTerm Syntax.SwitchCase -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.SwitchCase
switchCaseWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchCase"),
              Core.projectionField = (Core.Name "consequent")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

switchStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm [Syntax.SwitchCase] -> Phantoms.TTerm Syntax.SwitchStatement
switchStatement discriminant cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Phantoms.unTTerm discriminant)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

switchStatementCases :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm [Syntax.SwitchCase]
switchStatementCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementDiscriminant :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression
switchStatementDiscriminant x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
        Core.projectionField = (Core.Name "discriminant")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

switchStatementWithCases :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm [Syntax.SwitchCase] -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "discriminant")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

switchStatementWithDiscriminant :: Phantoms.TTerm Syntax.SwitchStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.SwitchStatement
switchStatementWithDiscriminant original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.SwitchStatement"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateElement :: Phantoms.TTerm String -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TemplateElement
templateElement value tail =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm tail)}]}))

templateElementTail :: Phantoms.TTerm Syntax.TemplateElement -> Phantoms.TTerm Bool
templateElementTail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
        Core.projectionField = (Core.Name "tail")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateElementValue :: Phantoms.TTerm Syntax.TemplateElement -> Phantoms.TTerm String
templateElementValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateElementWithTail :: Phantoms.TTerm Syntax.TemplateElement -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.TemplateElement
templateElementWithTail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

templateElementWithValue :: Phantoms.TTerm Syntax.TemplateElement -> Phantoms.TTerm String -> Phantoms.TTerm Syntax.TemplateElement
templateElementWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateElement"),
              Core.projectionField = (Core.Name "tail")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

templateLiteral :: Phantoms.TTerm [Syntax.TemplateElement] -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.TemplateLiteral
templateLiteral quasis expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Phantoms.unTTerm quasis)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

templateLiteralExpressions :: Phantoms.TTerm Syntax.TemplateLiteral -> Phantoms.TTerm [Syntax.Expression]
templateLiteralExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
        Core.projectionField = (Core.Name "expressions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateLiteralQuasis :: Phantoms.TTerm Syntax.TemplateLiteral -> Phantoms.TTerm [Syntax.TemplateElement]
templateLiteralQuasis x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
        Core.projectionField = (Core.Name "quasis")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

templateLiteralWithExpressions :: Phantoms.TTerm Syntax.TemplateLiteral -> Phantoms.TTerm [Syntax.Expression] -> Phantoms.TTerm Syntax.TemplateLiteral
templateLiteralWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
              Core.projectionField = (Core.Name "quasis")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

templateLiteralWithQuasis :: Phantoms.TTerm Syntax.TemplateLiteral -> Phantoms.TTerm [Syntax.TemplateElement] -> Phantoms.TTerm Syntax.TemplateLiteral
templateLiteralWithQuasis original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TemplateLiteral"),
              Core.projectionField = (Core.Name "expressions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

throwStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.ThrowStatement
throwStatement x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

tryStatement :: Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm (Maybe Syntax.CatchClause) -> Phantoms.TTerm (Maybe Syntax.BlockStatement) -> Phantoms.TTerm Syntax.TryStatement
tryStatement block handler finalizer =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Phantoms.unTTerm handler)},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Phantoms.unTTerm finalizer)}]}))

tryStatementBlock :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.BlockStatement
tryStatementBlock x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
        Core.projectionField = (Core.Name "block")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatementFinalizer :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm (Maybe Syntax.BlockStatement)
tryStatementFinalizer x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
        Core.projectionField = (Core.Name "finalizer")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatementHandler :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm (Maybe Syntax.CatchClause)
tryStatementHandler x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
        Core.projectionField = (Core.Name "handler")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

tryStatementWithBlock :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm Syntax.BlockStatement -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithBlock original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "handler")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "finalizer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tryStatementWithFinalizer :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm (Maybe Syntax.BlockStatement) -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithFinalizer original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "handler")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tryStatementWithHandler :: Phantoms.TTerm Syntax.TryStatement -> Phantoms.TTerm (Maybe Syntax.CatchClause) -> Phantoms.TTerm Syntax.TryStatement
tryStatementWithHandler original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "block")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TryStatement"),
              Core.projectionField = (Core.Name "finalizer")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeAnnotation :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypeAnnotation
typeAnnotation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.javaScript.syntax.TypeAnnotation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

typeExpressionAny :: Phantoms.TTerm Syntax.TypeExpression
typeExpressionAny =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

typeExpressionArray :: Phantoms.TTerm Syntax.ArrayTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionArray x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionFunction :: Phantoms.TTerm Syntax.FunctionTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionFunction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionIdentifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionLiteral :: Phantoms.TTerm Syntax.Literal -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionNever :: Phantoms.TTerm Syntax.TypeExpression
typeExpressionNever =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "never"),
        Core.fieldTerm = Core.TermUnit}}))

typeExpressionObject :: Phantoms.TTerm Syntax.ObjectTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionObject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionOptional :: Phantoms.TTerm Syntax.TypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionOptional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionParameterized :: Phantoms.TTerm Syntax.ParameterizedTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionParameterized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameterized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionUnion :: Phantoms.TTerm Syntax.UnionTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
typeExpressionUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

typeExpressionVoid :: Phantoms.TTerm Syntax.TypeExpression
typeExpressionVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

typeParameter :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.TypeParameter
typeParameter name constraint default_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Phantoms.unTTerm constraint)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm default_)}]}))

typeParameterConstraint :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeExpression)
typeParameterConstraint x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "constraint")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterDefault :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeExpression)
typeParameterDefault x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "default")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterName :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.Identifier
typeParameterName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeParameterWithConstraint :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithConstraint original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeParameterWithDefault :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm (Maybe Syntax.TypeExpression) -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithDefault original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "constraint")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeParameterWithName :: Phantoms.TTerm Syntax.TypeParameter -> Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm Syntax.TypeParameter
typeParameterWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "constraint")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.TypeParameter"),
              Core.projectionField = (Core.Name "default")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unArrayTypeExpression :: Phantoms.TTerm Syntax.ArrayTypeExpression -> Phantoms.TTerm Syntax.TypeExpression
unArrayTypeExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.ArrayTypeExpression")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIdentifier :: Phantoms.TTerm Syntax.Identifier -> Phantoms.TTerm String
unIdentifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.Identifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unImportDefaultSpecifier :: Phantoms.TTerm Syntax.ImportDefaultSpecifier -> Phantoms.TTerm Syntax.Identifier
unImportDefaultSpecifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.ImportDefaultSpecifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unImportNamespaceSpecifier :: Phantoms.TTerm Syntax.ImportNamespaceSpecifier -> Phantoms.TTerm Syntax.Identifier
unImportNamespaceSpecifier x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.ImportNamespaceSpecifier")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRestElement :: Phantoms.TTerm Syntax.RestElement -> Phantoms.TTerm Syntax.Pattern
unRestElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.RestElement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSpreadElement :: Phantoms.TTerm Syntax.SpreadElement -> Phantoms.TTerm Syntax.Expression
unSpreadElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.SpreadElement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unThrowStatement :: Phantoms.TTerm Syntax.ThrowStatement -> Phantoms.TTerm Syntax.Expression
unThrowStatement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.ThrowStatement")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTypeAnnotation :: Phantoms.TTerm Syntax.TypeAnnotation -> Phantoms.TTerm Syntax.TypeExpression
unTypeAnnotation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.javaScript.syntax.TypeAnnotation")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpression :: Phantoms.TTerm Syntax.UnaryOperator -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpression operator argument prefix =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)}]}))

unaryExpressionArgument :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.Expression
unaryExpressionArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
        Core.projectionField = (Core.Name "argument")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionOperator :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryOperator
unaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionPrefix :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Bool
unaryExpressionPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
        Core.projectionField = (Core.Name "prefix")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionWithArgument :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryExpressionWithOperator :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Syntax.UnaryOperator -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryExpressionWithPrefix :: Phantoms.TTerm Syntax.UnaryExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Syntax.UnaryExpression
unaryExpressionWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryExpression"),
              Core.projectionField = (Core.Name "argument")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryOperatorBitwiseNot :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorBitwiseNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseNot"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorDecrement :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorDecrement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decrement"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorDelete :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorDelete =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delete"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorIncrement :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorIncrement =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "increment"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorNegate :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorNegate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorNot :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorPlus :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorTypeof :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorTypeof =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeof"),
        Core.fieldTerm = Core.TermUnit}}))

unaryOperatorVoid :: Phantoms.TTerm Syntax.UnaryOperator
unaryOperatorVoid =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))

variableDeclaration :: Phantoms.TTerm Syntax.VariableKind -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclaration kind declarations =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm declarations)}]}))

variableDeclarationDeclarations :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator]
variableDeclarationDeclarations x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "declarations")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationKind :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.VariableKind
variableDeclarationKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
        Core.projectionField = (Core.Name "kind")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclarationWithDeclarations :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm [Syntax.VariableDeclarator] -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithDeclarations original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "kind")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableDeclarationWithKind :: Phantoms.TTerm Syntax.VariableDeclaration -> Phantoms.TTerm Syntax.VariableKind -> Phantoms.TTerm Syntax.VariableDeclaration
variableDeclarationWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclaration"),
              Core.projectionField = (Core.Name "declarations")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclarator :: Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclarator id init =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm init)}]}))

variableDeclaratorId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.Pattern
variableDeclaratorId x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "id")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorInit :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.Expression)
variableDeclaratorInit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
        Core.projectionField = (Core.Name "init")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableDeclaratorWithId :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm Syntax.Pattern -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithId original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "init")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableDeclaratorWithInit :: Phantoms.TTerm Syntax.VariableDeclarator -> Phantoms.TTerm (Maybe Syntax.Expression) -> Phantoms.TTerm Syntax.VariableDeclarator
variableDeclaratorWithInit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableDeclarator"),
              Core.projectionField = (Core.Name "id")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableKindConst :: Phantoms.TTerm Syntax.VariableKind
variableKindConst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = Core.TermUnit}}))

variableKindLet :: Phantoms.TTerm Syntax.VariableKind
variableKindLet =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = Core.TermUnit}}))

variableKindVar :: Phantoms.TTerm Syntax.VariableKind
variableKindVar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.javaScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))

whileStatement :: Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatement test body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

whileStatementBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement
whileStatementBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "body")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementTest :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Expression
whileStatementTest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
        Core.projectionField = (Core.Name "test")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

whileStatementWithBody :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Statement -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "test")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

whileStatementWithTest :: Phantoms.TTerm Syntax.WhileStatement -> Phantoms.TTerm Syntax.Expression -> Phantoms.TTerm Syntax.WhileStatement
whileStatementWithTest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.javaScript.syntax.WhileStatement"),
              Core.projectionField = (Core.Name "body")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
