-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.typeScript.syntax

module Hydra.Dsl.TypeScript.Syntax where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.TypeScript.Syntax as Syntax
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Int as I
-- | DSL injection for the expression variant of hydra.typeScript.syntax.ArrayElement
arrayElementExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ArrayElement
arrayElementExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the hole variant of hydra.typeScript.syntax.ArrayElement
arrayElementHole :: Typed.TypedTerm Syntax.ArrayElement
arrayElementHole =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hole"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the spread variant of hydra.typeScript.syntax.ArrayElement
arrayElementSpread :: Typed.TypedTerm Syntax.SpreadElement -> Typed.TypedTerm Syntax.ArrayElement
arrayElementSpread x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrayElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "spread"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.typeScript.syntax.ArrayTypeExpression wrapper
arrayTypeExpression :: Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.ArrayTypeExpression
arrayTypeExpression x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.ArrayTypeExpression"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the block variant of hydra.typeScript.syntax.ArrowFunctionBody
arrowFunctionBodyBlock :: Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.ArrowFunctionBody
arrowFunctionBodyBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the expression variant of hydra.typeScript.syntax.ArrowFunctionBody
arrowFunctionBodyExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ArrowFunctionBody
arrowFunctionBodyExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionBody"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpression :: Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ArrowFunctionBody -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ArrowFunctionExpression
arrowFunctionExpression params body async =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)}]}))
-- | DSL accessor for the async field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionAsync :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm Bool
arrowFunctionExpressionAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionBody :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm Syntax.ArrowFunctionBody
arrowFunctionExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionParams :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm [Syntax.Pattern]
arrowFunctionExpressionParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionWithAsync :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionWithBody :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm Syntax.ArrowFunctionBody -> Typed.TypedTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.typeScript.syntax.ArrowFunctionExpression
arrowFunctionExpressionWithParams :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.ArrowFunctionExpression
arrowFunctionExpressionWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ArrowFunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.AsExpression
asExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.AsExpression
asExpression expression type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the expression field of hydra.typeScript.syntax.AsExpression
asExpressionExpression :: Typed.TypedTerm Syntax.AsExpression -> Typed.TypedTerm Syntax.Expression
asExpressionExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typeScript.syntax.AsExpression
asExpressionType :: Typed.TypedTerm Syntax.AsExpression -> Typed.TypedTerm Syntax.TypeExpression
asExpressionType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expression field of hydra.typeScript.syntax.AsExpression
asExpressionWithExpression :: Typed.TypedTerm Syntax.AsExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AsExpression
asExpressionWithExpression original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typeScript.syntax.AsExpression
asExpressionWithType :: Typed.TypedTerm Syntax.AsExpression -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.AsExpression
asExpressionWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AsExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.AssignmentExpression
assignmentExpression :: Typed.TypedTerm Syntax.AssignmentOperator -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpression operator left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionLeft :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Pattern
assignmentExpressionLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionOperator :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.AssignmentOperator
assignmentExpressionOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionRight :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression
assignmentExpressionRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionWithLeft :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionWithOperator :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.AssignmentOperator -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typeScript.syntax.AssignmentExpression
assignmentExpressionWithRight :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentExpression
assignmentExpressionWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the addAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorAddAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorAddAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "addAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the andAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorAndAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorAndAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "andAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the assign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAndAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorBitwiseAndAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseAndAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAndAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOrAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorBitwiseOrAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseOrAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOrAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXorAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorBitwiseXorAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorBitwiseXorAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXorAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divideAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorDivideAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorDivideAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divideAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the exponentiateAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorExponentiateAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorExponentiateAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exponentiateAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShiftAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorLeftShiftAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorLeftShiftAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the moduloAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorModuloAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorModuloAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "moduloAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiplyAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorMultiplyAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorMultiplyAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiplyAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nullishAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorNullishAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorNullishAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullishAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the orAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorOrAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorOrAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShiftAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorRightShiftAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorRightShiftAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtractAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorSubtractAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorSubtractAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtractAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unsignedRightShiftAssign variant of hydra.typeScript.syntax.AssignmentOperator
assignmentOperatorUnsignedRightShiftAssign :: Typed.TypedTerm Syntax.AssignmentOperator
assignmentOperatorUnsignedRightShiftAssign =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedRightShiftAssign"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.AssignmentPattern
assignmentPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentPattern
assignmentPattern left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.typeScript.syntax.AssignmentPattern
assignmentPatternLeft :: Typed.TypedTerm Syntax.AssignmentPattern -> Typed.TypedTerm Syntax.Pattern
assignmentPatternLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typeScript.syntax.AssignmentPattern
assignmentPatternRight :: Typed.TypedTerm Syntax.AssignmentPattern -> Typed.TypedTerm Syntax.Expression
assignmentPatternRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.typeScript.syntax.AssignmentPattern
assignmentPatternWithLeft :: Typed.TypedTerm Syntax.AssignmentPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.AssignmentPattern
assignmentPatternWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typeScript.syntax.AssignmentPattern
assignmentPatternWithRight :: Typed.TypedTerm Syntax.AssignmentPattern -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.AssignmentPattern
assignmentPatternWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.AssignmentPattern"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.BinaryExpression
binaryExpression :: Typed.TypedTerm Syntax.BinaryOperator -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpression
binaryExpression operator left right =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)}]}))
-- | DSL accessor for the left field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionLeft :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.Expression
binaryExpressionLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionOperator :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.BinaryOperator
binaryExpressionOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionRight :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.Expression
binaryExpressionRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the left field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionWithLeft :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpression
binaryExpressionWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionWithOperator :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.BinaryOperator -> Typed.TypedTerm Syntax.BinaryExpression
binaryExpressionWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typeScript.syntax.BinaryExpression
binaryExpressionWithRight :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.BinaryExpression
binaryExpressionWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the add variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorAdd :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorAdd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the and variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorAnd :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseAnd variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorBitwiseAnd :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorBitwiseAnd =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseAnd"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseOr variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorBitwiseOr :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorBitwiseOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseOr"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the bitwiseXor variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorBitwiseXor :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorBitwiseXor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseXor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the divide variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorDivide :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorDivide =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the equal variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the exponentiate variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorExponentiate :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorExponentiate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "exponentiate"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThan variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorGreaterThan :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorGreaterThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the greaterThanOrEqual variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorGreaterThanOrEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorGreaterThanOrEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the in variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorIn :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorIn =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "in"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the instanceof variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorInstanceof :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorInstanceof =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "instanceof"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the leftShift variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorLeftShift :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorLeftShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThan variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorLessThan :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorLessThan =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the lessThanOrEqual variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorLessThanOrEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorLessThanOrEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the modulo variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorModulo :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorModulo =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the multiply variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorMultiply :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorMultiply =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the notEqual variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorNotEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorNotEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nullishCoalescing variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorNullishCoalescing :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorNullishCoalescing =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nullishCoalescing"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the or variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorOr :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorOr =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the rightShift variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorRightShift :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorRightShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictEqual variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorStrictEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorStrictEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the strictNotEqual variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorStrictNotEqual :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorStrictNotEqual =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strictNotEqual"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the subtract variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorSubtract :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorSubtract =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unsignedRightShift variant of hydra.typeScript.syntax.BinaryOperator
binaryOperatorUnsignedRightShift :: Typed.TypedTerm Syntax.BinaryOperator
binaryOperatorUnsignedRightShift =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedRightShift"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.CallExpression
callExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.CallExpression
callExpression callee arguments optional =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Typed.unTypedTerm callee)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm optional)}]}))
-- | DSL accessor for the arguments field of hydra.typeScript.syntax.CallExpression
callExpressionArguments :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm [Syntax.Expression]
callExpressionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the callee field of hydra.typeScript.syntax.CallExpression
callExpressionCallee :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Syntax.Expression
callExpressionCallee x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
        Core.projectionFieldName = (Core.Name "callee")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the optional field of hydra.typeScript.syntax.CallExpression
callExpressionOptional :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Bool
callExpressionOptional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
        Core.projectionFieldName = (Core.Name "optional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.typeScript.syntax.CallExpression
callExpressionWithArguments :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.CallExpression
callExpressionWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "callee")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the callee field of hydra.typeScript.syntax.CallExpression
callExpressionWithCallee :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.CallExpression
callExpressionWithCallee original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the optional field of hydra.typeScript.syntax.CallExpression
callExpressionWithOptional :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.CallExpression
callExpressionWithOptional original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "callee"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "callee")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CallExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.CatchClause
catchClause :: Typed.TypedTerm (Maybe Syntax.Pattern) -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.CatchClause
catchClause param body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm param)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.CatchClause
catchClauseBody :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm Syntax.BlockStatement
catchClauseBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the param field of hydra.typeScript.syntax.CatchClause
catchClauseParam :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm (Maybe Syntax.Pattern)
catchClauseParam x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
        Core.projectionFieldName = (Core.Name "param")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.CatchClause
catchClauseWithBody :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.CatchClause
catchClauseWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
              Core.projectionFieldName = (Core.Name "param")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the param field of hydra.typeScript.syntax.CatchClause
catchClauseWithParam :: Typed.TypedTerm Syntax.CatchClause -> Typed.TypedTerm (Maybe Syntax.Pattern) -> Typed.TypedTerm Syntax.CatchClause
catchClauseWithParam original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "param"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.CatchClause"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.ClassDeclaration
classDeclaration :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ClassBody -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclaration id superClass body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Typed.unTypedTerm superClass)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationBody :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.ClassBody
classDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the id field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationId :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.Identifier
classDeclarationId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the superClass field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationSuperClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm (Maybe Syntax.Expression)
classDeclarationSuperClass x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
        Core.projectionFieldName = (Core.Name "superClass")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationWithBody :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.ClassBody -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "superClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.ClassDeclarationWithComments
classDeclarationWithComments :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.ClassDeclarationWithComments
classDeclarationWithComments body comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ClassDeclarationWithComments
classDeclarationWithCommentsBody :: Typed.TypedTerm Syntax.ClassDeclarationWithComments -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationWithCommentsBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the comments field of hydra.typeScript.syntax.ClassDeclarationWithComments
classDeclarationWithCommentsComments :: Typed.TypedTerm Syntax.ClassDeclarationWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment)
classDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithBody :: Typed.TypedTerm Syntax.ClassDeclarationWithComments -> Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the comments field of hydra.typeScript.syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithComments :: Typed.TypedTerm Syntax.ClassDeclarationWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.ClassDeclarationWithComments
classDeclarationWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationWithId :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "superClass")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the superClass field of hydra.typeScript.syntax.ClassDeclaration
classDeclarationWithSuperClass :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ClassDeclaration
classDeclarationWithSuperClass original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "superClass"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ClassDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the block variant of hydra.typeScript.syntax.Comment
commentBlock :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Comment
commentBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the documentation variant of hydra.typeScript.syntax.Comment
commentDocumentation :: Typed.TypedTerm Syntax.DocumentationComment -> Typed.TypedTerm Syntax.Comment
commentDocumentation x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "documentation"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the line variant of hydra.typeScript.syntax.Comment
commentLine :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Comment
commentLine x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Comment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "line"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ConditionalExpression
conditionalExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpression test consequent alternate =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm consequent)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Typed.unTypedTerm alternate)}]}))
-- | DSL accessor for the alternate field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionAlternate :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression
conditionalExpressionAlternate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
        Core.projectionFieldName = (Core.Name "alternate")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the consequent field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionConsequent :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression
conditionalExpressionConsequent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
        Core.projectionFieldName = (Core.Name "consequent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionTest :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression
conditionalExpressionTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the alternate field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionWithAlternate :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionWithAlternate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the consequent field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionWithConsequent :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionWithConsequent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "alternate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.ConditionalExpression
conditionalExpressionWithTest :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ConditionalExpression
conditionalExpressionWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ConditionalExpression"),
              Core.projectionFieldName = (Core.Name "alternate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.DoWhileStatement
doWhileStatement :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DoWhileStatement
doWhileStatement body test =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.DoWhileStatement
doWhileStatementBody :: Typed.TypedTerm Syntax.DoWhileStatement -> Typed.TypedTerm Syntax.Statement
doWhileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.DoWhileStatement
doWhileStatementTest :: Typed.TypedTerm Syntax.DoWhileStatement -> Typed.TypedTerm Syntax.Expression
doWhileStatementTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.DoWhileStatement
doWhileStatementWithBody :: Typed.TypedTerm Syntax.DoWhileStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.DoWhileStatement
doWhileStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.DoWhileStatement
doWhileStatementWithTest :: Typed.TypedTerm Syntax.DoWhileStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.DoWhileStatement
doWhileStatementWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DoWhileStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.DocumentationComment
documentationComment :: Typed.TypedTerm String -> Typed.TypedTerm [Syntax.DocumentationTag] -> Typed.TypedTerm Syntax.DocumentationComment
documentationComment description tags =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Typed.unTypedTerm tags)}]}))
-- | DSL accessor for the description field of hydra.typeScript.syntax.DocumentationComment
documentationCommentDescription :: Typed.TypedTerm Syntax.DocumentationComment -> Typed.TypedTerm String
documentationCommentDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the tags field of hydra.typeScript.syntax.DocumentationComment
documentationCommentTags :: Typed.TypedTerm Syntax.DocumentationComment -> Typed.TypedTerm [Syntax.DocumentationTag]
documentationCommentTags x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
        Core.projectionFieldName = (Core.Name "tags")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.typeScript.syntax.DocumentationComment
documentationCommentWithDescription :: Typed.TypedTerm Syntax.DocumentationComment -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.DocumentationComment
documentationCommentWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
              Core.projectionFieldName = (Core.Name "tags")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the tags field of hydra.typeScript.syntax.DocumentationComment
documentationCommentWithTags :: Typed.TypedTerm Syntax.DocumentationComment -> Typed.TypedTerm [Syntax.DocumentationTag] -> Typed.TypedTerm Syntax.DocumentationComment
documentationCommentWithTags original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationComment"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.DocumentationTag
documentationTag :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.DocumentationTag
documentationTag name type_ paramName description =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Typed.unTypedTerm paramName)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)}]}))
-- | DSL accessor for the description field of hydra.typeScript.syntax.DocumentationTag
documentationTagDescription :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm String
documentationTagDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typeScript.syntax.DocumentationTag
documentationTagName :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm String
documentationTagName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the paramName field of hydra.typeScript.syntax.DocumentationTag
documentationTagParamName :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm (Maybe Syntax.Identifier)
documentationTagParamName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
        Core.projectionFieldName = (Core.Name "paramName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typeScript.syntax.DocumentationTag
documentationTagType :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm (Maybe Syntax.TypeExpression)
documentationTagType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the description field of hydra.typeScript.syntax.DocumentationTag
documentationTagWithDescription :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.DocumentationTag
documentationTagWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "paramName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typeScript.syntax.DocumentationTag
documentationTagWithName :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.DocumentationTag
documentationTagWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "paramName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the paramName field of hydra.typeScript.syntax.DocumentationTag
documentationTagWithParamName :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.DocumentationTag
documentationTagWithParamName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typeScript.syntax.DocumentationTag
documentationTagWithType :: Typed.TypedTerm Syntax.DocumentationTag -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm Syntax.DocumentationTag
documentationTagWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paramName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "paramName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.DocumentationTag"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.ExportAllDeclaration
exportAllDeclaration :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.ExportAllDeclaration
exportAllDeclaration exported source =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Typed.unTypedTerm exported)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)}]}))
-- | DSL accessor for the exported field of hydra.typeScript.syntax.ExportAllDeclaration
exportAllDeclarationExported :: Typed.TypedTerm Syntax.ExportAllDeclaration -> Typed.TypedTerm (Maybe Syntax.Identifier)
exportAllDeclarationExported x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
        Core.projectionFieldName = (Core.Name "exported")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the source field of hydra.typeScript.syntax.ExportAllDeclaration
exportAllDeclarationSource :: Typed.TypedTerm Syntax.ExportAllDeclaration -> Typed.TypedTerm Syntax.StringLiteral
exportAllDeclarationSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the exported field of hydra.typeScript.syntax.ExportAllDeclaration
exportAllDeclarationWithExported :: Typed.TypedTerm Syntax.ExportAllDeclaration -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.ExportAllDeclaration
exportAllDeclarationWithExported original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the source field of hydra.typeScript.syntax.ExportAllDeclaration
exportAllDeclarationWithSource :: Typed.TypedTerm Syntax.ExportAllDeclaration -> Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.ExportAllDeclaration
exportAllDeclarationWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportAllDeclaration"),
              Core.projectionFieldName = (Core.Name "exported")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the all variant of hydra.typeScript.syntax.ExportDeclaration
exportDeclarationAll :: Typed.TypedTerm Syntax.ExportAllDeclaration -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclarationAll x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the declaration variant of hydra.typeScript.syntax.ExportDeclaration
exportDeclarationDeclaration :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclarationDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "declaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the default variant of hydra.typeScript.syntax.ExportDeclaration
exportDeclarationDefault :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclarationDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.typeScript.syntax.ExportDeclaration
exportDeclarationNamed :: Typed.TypedTerm Syntax.NamedExport -> Typed.TypedTerm Syntax.ExportDeclaration
exportDeclarationNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportDeclaration"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ExportSpecifier
exportSpecifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ExportSpecifier
exportSpecifier local exported =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm local)},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Typed.unTypedTerm exported)}]}))
-- | DSL accessor for the exported field of hydra.typeScript.syntax.ExportSpecifier
exportSpecifierExported :: Typed.TypedTerm Syntax.ExportSpecifier -> Typed.TypedTerm Syntax.Identifier
exportSpecifierExported x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
        Core.projectionFieldName = (Core.Name "exported")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the local field of hydra.typeScript.syntax.ExportSpecifier
exportSpecifierLocal :: Typed.TypedTerm Syntax.ExportSpecifier -> Typed.TypedTerm Syntax.Identifier
exportSpecifierLocal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the exported field of hydra.typeScript.syntax.ExportSpecifier
exportSpecifierWithExported :: Typed.TypedTerm Syntax.ExportSpecifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ExportSpecifier
exportSpecifierWithExported original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the local field of hydra.typeScript.syntax.ExportSpecifier
exportSpecifierWithLocal :: Typed.TypedTerm Syntax.ExportSpecifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ExportSpecifier
exportSpecifierWithLocal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "exported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ExportSpecifier"),
              Core.projectionFieldName = (Core.Name "exported")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.typeScript.syntax.Expression
expressionArray :: Typed.TypedTerm Syntax.ArrayExpression -> Typed.TypedTerm Syntax.Expression
expressionArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the arrow variant of hydra.typeScript.syntax.Expression
expressionArrow :: Typed.TypedTerm Syntax.ArrowFunctionExpression -> Typed.TypedTerm Syntax.Expression
expressionArrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "arrow"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the asExpression variant of hydra.typeScript.syntax.Expression
expressionAsExpression :: Typed.TypedTerm Syntax.AsExpression -> Typed.TypedTerm Syntax.Expression
expressionAsExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "asExpression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assignment variant of hydra.typeScript.syntax.Expression
expressionAssignment :: Typed.TypedTerm Syntax.AssignmentExpression -> Typed.TypedTerm Syntax.Expression
expressionAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the await variant of hydra.typeScript.syntax.Expression
expressionAwait :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression
expressionAwait x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "await"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the binary variant of hydra.typeScript.syntax.Expression
expressionBinary :: Typed.TypedTerm Syntax.BinaryExpression -> Typed.TypedTerm Syntax.Expression
expressionBinary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the call variant of hydra.typeScript.syntax.Expression
expressionCall :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Syntax.Expression
expressionCall x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "call"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the conditional variant of hydra.typeScript.syntax.Expression
expressionConditional :: Typed.TypedTerm Syntax.ConditionalExpression -> Typed.TypedTerm Syntax.Expression
expressionConditional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "conditional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.typeScript.syntax.Expression
expressionFunction :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Syntax.Expression
expressionFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the identifier variant of hydra.typeScript.syntax.Expression
expressionIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Expression
expressionIdentifier x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.typeScript.syntax.Expression
expressionLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.Expression
expressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the member variant of hydra.typeScript.syntax.Expression
expressionMember :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Syntax.Expression
expressionMember x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "member"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the new variant of hydra.typeScript.syntax.Expression
expressionNew :: Typed.TypedTerm Syntax.CallExpression -> Typed.TypedTerm Syntax.Expression
expressionNew x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "new"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the object variant of hydra.typeScript.syntax.Expression
expressionObject :: Typed.TypedTerm Syntax.ObjectExpression -> Typed.TypedTerm Syntax.Expression
expressionObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parenthesized variant of hydra.typeScript.syntax.Expression
expressionParenthesized :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression
expressionParenthesized x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the sequence variant of hydra.typeScript.syntax.Expression
expressionSequence :: Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.Expression
expressionSequence x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sequence"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the spread variant of hydra.typeScript.syntax.Expression
expressionSpread :: Typed.TypedTerm Syntax.SpreadElement -> Typed.TypedTerm Syntax.Expression
expressionSpread x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "spread"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the this variant of hydra.typeScript.syntax.Expression
expressionThis :: Typed.TypedTerm Syntax.Expression
expressionThis =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "this"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the unary variant of hydra.typeScript.syntax.Expression
expressionUnary :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.Expression
expressionUnary x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the yield variant of hydra.typeScript.syntax.Expression
expressionYield :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.Expression
expressionYield x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "yield"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the pattern variant of hydra.typeScript.syntax.ForInLeft
forInLeftPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.ForInLeft
forInLeftPattern x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInLeft"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.typeScript.syntax.ForInLeft
forInLeftVariable :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm Syntax.ForInLeft
forInLeftVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInLeft"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ForInStatement
forInStatement :: Typed.TypedTerm Syntax.ForInLeft -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForInStatement
forInStatement left right body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ForInStatement
forInStatementBody :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.Statement
forInStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the left field of hydra.typeScript.syntax.ForInStatement
forInStatementLeft :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.ForInLeft
forInStatementLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typeScript.syntax.ForInStatement
forInStatementRight :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.Expression
forInStatementRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ForInStatement
forInStatementWithBody :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForInStatement
forInStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the left field of hydra.typeScript.syntax.ForInStatement
forInStatementWithLeft :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.ForInLeft -> Typed.TypedTerm Syntax.ForInStatement
forInStatementWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typeScript.syntax.ForInStatement
forInStatementWithRight :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ForInStatement
forInStatementWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the expression variant of hydra.typeScript.syntax.ForInit
forInitExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ForInit
forInitExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variable variant of hydra.typeScript.syntax.ForInit
forInitVariable :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm Syntax.ForInit
forInitVariable x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ForInit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ForOfStatement
forOfStatement :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ForInLeft -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForOfStatement
forOfStatement await left right body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Typed.unTypedTerm await)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the await field of hydra.typeScript.syntax.ForOfStatement
forOfStatementAwait :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Bool
forOfStatementAwait x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
        Core.projectionFieldName = (Core.Name "await")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ForOfStatement
forOfStatementBody :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.Statement
forOfStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the left field of hydra.typeScript.syntax.ForOfStatement
forOfStatementLeft :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.ForInLeft
forOfStatementLeft x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the right field of hydra.typeScript.syntax.ForOfStatement
forOfStatementRight :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.Expression
forOfStatementRight x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the await field of hydra.typeScript.syntax.ForOfStatement
forOfStatementWithAwait :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.ForOfStatement
forOfStatementWithAwait original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ForOfStatement
forOfStatementWithBody :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForOfStatement
forOfStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "await")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the left field of hydra.typeScript.syntax.ForOfStatement
forOfStatementWithLeft :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.ForInLeft -> Typed.TypedTerm Syntax.ForOfStatement
forOfStatementWithLeft original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "await")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the right field of hydra.typeScript.syntax.ForOfStatement
forOfStatementWithRight :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ForOfStatement
forOfStatementWithRight original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "await"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "await")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForOfStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.ForStatement
forStatement :: Typed.TypedTerm (Maybe Syntax.ForInit) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForStatement
forStatement init test update body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Typed.unTypedTerm update)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ForStatement
forStatementBody :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Statement
forStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.typeScript.syntax.ForStatement
forStatementInit :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.ForInit)
forStatementInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.ForStatement
forStatementTest :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
forStatementTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the update field of hydra.typeScript.syntax.ForStatement
forStatementUpdate :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Expression)
forStatementUpdate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
        Core.projectionFieldName = (Core.Name "update")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ForStatement
forStatementWithBody :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ForStatement
forStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "update")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the init field of hydra.typeScript.syntax.ForStatement
forStatementWithInit :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.ForInit) -> Typed.TypedTerm Syntax.ForStatement
forStatementWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "update")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.ForStatement
forStatementWithTest :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ForStatement
forStatementWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "update")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the update field of hydra.typeScript.syntax.ForStatement
forStatementWithUpdate :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.ForStatement
forStatementWithUpdate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "update"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ForStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.FunctionDeclaration
functionDeclaration :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclaration id params body async generator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Typed.unTypedTerm generator)}]}))
-- | DSL accessor for the async field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationAsync :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Bool
functionDeclarationAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationBody :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.BlockStatement
functionDeclarationBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the generator field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationGenerator :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Bool
functionDeclarationGenerator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
        Core.projectionFieldName = (Core.Name "generator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the id field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationId :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.Identifier
functionDeclarationId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationParams :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm [Syntax.Pattern]
functionDeclarationParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationWithAsync :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationWithBody :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.FunctionDeclarationWithComments
functionDeclarationWithComments :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithComments body comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsBody :: Typed.TypedTerm Syntax.FunctionDeclarationWithComments -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithCommentsBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the comments field of hydra.typeScript.syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsComments :: Typed.TypedTerm Syntax.FunctionDeclarationWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment)
functionDeclarationWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithBody :: Typed.TypedTerm Syntax.FunctionDeclarationWithComments -> Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the comments field of hydra.typeScript.syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithComments :: Typed.TypedTerm Syntax.FunctionDeclarationWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.FunctionDeclarationWithComments
functionDeclarationWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclarationWithComments"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the generator field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationWithGenerator :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithGenerator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationWithId :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.typeScript.syntax.FunctionDeclaration
functionDeclarationWithParams :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.FunctionDeclaration
functionDeclarationWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionDeclaration"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.FunctionExpression
functionExpression :: Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionExpression
functionExpression id params body async generator =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm params)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm async)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Typed.unTypedTerm generator)}]}))
-- | DSL accessor for the async field of hydra.typeScript.syntax.FunctionExpression
functionExpressionAsync :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Bool
functionExpressionAsync x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
        Core.projectionFieldName = (Core.Name "async")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.FunctionExpression
functionExpressionBody :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Syntax.BlockStatement
functionExpressionBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the generator field of hydra.typeScript.syntax.FunctionExpression
functionExpressionGenerator :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Bool
functionExpressionGenerator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
        Core.projectionFieldName = (Core.Name "generator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the id field of hydra.typeScript.syntax.FunctionExpression
functionExpressionId :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm (Maybe Syntax.Identifier)
functionExpressionId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the params field of hydra.typeScript.syntax.FunctionExpression
functionExpressionParams :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm [Syntax.Pattern]
functionExpressionParams x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
        Core.projectionFieldName = (Core.Name "params")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the async field of hydra.typeScript.syntax.FunctionExpression
functionExpressionWithAsync :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionExpression
functionExpressionWithAsync original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the body field of hydra.typeScript.syntax.FunctionExpression
functionExpressionWithBody :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.FunctionExpression
functionExpressionWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the generator field of hydra.typeScript.syntax.FunctionExpression
functionExpressionWithGenerator :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.FunctionExpression
functionExpressionWithGenerator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the id field of hydra.typeScript.syntax.FunctionExpression
functionExpressionWithId :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm (Maybe Syntax.Identifier) -> Typed.TypedTerm Syntax.FunctionExpression
functionExpressionWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "params")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the params field of hydra.typeScript.syntax.FunctionExpression
functionExpressionWithParams :: Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm [Syntax.Pattern] -> Typed.TypedTerm Syntax.FunctionExpression
functionExpressionWithParams original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "params"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "async"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "async")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionExpression"),
              Core.projectionFieldName = (Core.Name "generator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpression :: Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.FunctionTypeExpression
functionTypeExpression typeParameters parameters returnType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm typeParameters)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Typed.unTypedTerm returnType)}]}))
-- | DSL accessor for the parameters field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionParameters :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm [Syntax.TypeExpression]
functionTypeExpressionParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
        Core.projectionFieldName = (Core.Name "parameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the returnType field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionReturnType :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
functionTypeExpressionReturnType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
        Core.projectionFieldName = (Core.Name "returnType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParameters field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionTypeParameters :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm [Syntax.TypeParameter]
functionTypeExpressionTypeParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
        Core.projectionFieldName = (Core.Name "typeParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the parameters field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionWithParameters :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the returnType field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionWithReturnType :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithReturnType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeParameters field of hydra.typeScript.syntax.FunctionTypeExpression
functionTypeExpressionWithTypeParameters :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.FunctionTypeExpression
functionTypeExpressionWithTypeParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "parameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "returnType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.FunctionTypeExpression"),
              Core.projectionFieldName = (Core.Name "returnType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typeScript.syntax.Identifier wrapper
identifier :: Typed.TypedTerm String -> Typed.TypedTerm Syntax.Identifier
identifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.Identifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typeScript.syntax.IfStatement
ifStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm (Maybe Syntax.Statement) -> Typed.TypedTerm Syntax.IfStatement
ifStatement test consequent alternate =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm consequent)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Typed.unTypedTerm alternate)}]}))
-- | DSL accessor for the alternate field of hydra.typeScript.syntax.IfStatement
ifStatementAlternate :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm (Maybe Syntax.Statement)
ifStatementAlternate x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "alternate")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the consequent field of hydra.typeScript.syntax.IfStatement
ifStatementConsequent :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Statement
ifStatementConsequent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "consequent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.IfStatement
ifStatementTest :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Expression
ifStatementTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the alternate field of hydra.typeScript.syntax.IfStatement
ifStatementWithAlternate :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm (Maybe Syntax.Statement) -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithAlternate original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the consequent field of hydra.typeScript.syntax.IfStatement
ifStatementWithConsequent :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithConsequent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "alternate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.IfStatement
ifStatementWithTest :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.IfStatement
ifStatementWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.IfStatement"),
              Core.projectionFieldName = (Core.Name "alternate")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the default variant of hydra.typeScript.syntax.ImportClause
importClauseDefault :: Typed.TypedTerm Syntax.ImportDefaultSpecifier -> Typed.TypedTerm Syntax.ImportClause
importClauseDefault x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "default"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the named variant of hydra.typeScript.syntax.ImportClause
importClauseNamed :: Typed.TypedTerm Syntax.ImportSpecifier -> Typed.TypedTerm Syntax.ImportClause
importClauseNamed x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "named"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the namespace variant of hydra.typeScript.syntax.ImportClause
importClauseNamespace :: Typed.TypedTerm Syntax.ImportNamespaceSpecifier -> Typed.TypedTerm Syntax.ImportClause
importClauseNamespace x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "namespace"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ImportDeclaration
importDeclaration :: Typed.TypedTerm [Syntax.ImportClause] -> Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclaration specifiers source =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Typed.unTypedTerm specifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)}]}))
-- | DSL accessor for the source field of hydra.typeScript.syntax.ImportDeclaration
importDeclarationSource :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.StringLiteral
importDeclarationSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the specifiers field of hydra.typeScript.syntax.ImportDeclaration
importDeclarationSpecifiers :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm [Syntax.ImportClause]
importDeclarationSpecifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
        Core.projectionFieldName = (Core.Name "specifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the source field of hydra.typeScript.syntax.ImportDeclaration
importDeclarationWithSource :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "specifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the specifiers field of hydra.typeScript.syntax.ImportDeclaration
importDeclarationWithSpecifiers :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm [Syntax.ImportClause] -> Typed.TypedTerm Syntax.ImportDeclaration
importDeclarationWithSpecifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportDeclaration"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typeScript.syntax.ImportDefaultSpecifier wrapper
importDefaultSpecifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportDefaultSpecifier
importDefaultSpecifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.ImportDefaultSpecifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.typeScript.syntax.ImportNamespaceSpecifier wrapper
importNamespaceSpecifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportNamespaceSpecifier
importNamespaceSpecifier x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.ImportNamespaceSpecifier"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typeScript.syntax.ImportSpecifier
importSpecifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportSpecifier
importSpecifier imported local =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Typed.unTypedTerm imported)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm local)}]}))
-- | DSL accessor for the imported field of hydra.typeScript.syntax.ImportSpecifier
importSpecifierImported :: Typed.TypedTerm Syntax.ImportSpecifier -> Typed.TypedTerm Syntax.Identifier
importSpecifierImported x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
        Core.projectionFieldName = (Core.Name "imported")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the local field of hydra.typeScript.syntax.ImportSpecifier
importSpecifierLocal :: Typed.TypedTerm Syntax.ImportSpecifier -> Typed.TypedTerm Syntax.Identifier
importSpecifierLocal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the imported field of hydra.typeScript.syntax.ImportSpecifier
importSpecifierWithImported :: Typed.TypedTerm Syntax.ImportSpecifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportSpecifier
importSpecifierWithImported original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the local field of hydra.typeScript.syntax.ImportSpecifier
importSpecifierWithLocal :: Typed.TypedTerm Syntax.ImportSpecifier -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.ImportSpecifier
importSpecifierWithLocal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "imported"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ImportSpecifier"),
              Core.projectionFieldName = (Core.Name "imported")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclaration :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm [Syntax.PropertySignature] -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclaration name typeParameters extends members =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm typeParameters)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm extends)},
        Core.Field {
          Core.fieldName = (Core.Name "members"),
          Core.fieldTerm = (Typed.unTypedTerm members)}]}))
-- | DSL accessor for the extends field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationExtends :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeExpression]
interfaceDeclarationExtends x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "extends")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the members field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationMembers :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.PropertySignature]
interfaceDeclarationMembers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "members")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationName :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.Identifier
interfaceDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParameters field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationTypeParameters :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeParameter]
interfaceDeclarationTypeParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
        Core.projectionFieldName = (Core.Name "typeParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the extends field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationWithExtends :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationWithExtends original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "members"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "members")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the members field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationWithMembers :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.PropertySignature] -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationWithMembers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "members"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationWithName :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "members"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "members")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeParameters field of hydra.typeScript.syntax.InterfaceDeclaration
interfaceDeclarationWithTypeParameters :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.InterfaceDeclaration
interfaceDeclarationWithTypeParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "extends"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "extends")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "members"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.InterfaceDeclaration"),
              Core.projectionFieldName = (Core.Name "members")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.LabeledStatement
labeledStatement :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStatement
labeledStatement label body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.LabeledStatement
labeledStatementBody :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement
labeledStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the label field of hydra.typeScript.syntax.LabeledStatement
labeledStatementLabel :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Identifier
labeledStatementLabel x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.LabeledStatement
labeledStatementWithBody :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.LabeledStatement
labeledStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the label field of hydra.typeScript.syntax.LabeledStatement
labeledStatementWithLabel :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.LabeledStatement
labeledStatementWithLabel original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.LabeledStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the bigInt variant of hydra.typeScript.syntax.Literal
literalBigInt :: Typed.TypedTerm Integer -> Typed.TypedTerm Syntax.Literal
literalBigInt x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bigInt"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the boolean variant of hydra.typeScript.syntax.Literal
literalBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Literal
literalBoolean x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the null variant of hydra.typeScript.syntax.Literal
literalNull :: Typed.TypedTerm Syntax.Literal
literalNull =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the number variant of hydra.typeScript.syntax.Literal
literalNumber :: Typed.TypedTerm Syntax.NumericLiteral -> Typed.TypedTerm Syntax.Literal
literalNumber x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the string variant of hydra.typeScript.syntax.Literal
literalString :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Syntax.Literal
literalString x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the template variant of hydra.typeScript.syntax.Literal
literalTemplate :: Typed.TypedTerm Syntax.TemplateLiteral -> Typed.TypedTerm Syntax.Literal
literalTemplate x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "template"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the undefined variant of hydra.typeScript.syntax.Literal
literalUndefined :: Typed.TypedTerm Syntax.Literal
literalUndefined =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undefined"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.MemberExpression
memberExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MemberExpression
memberExpression object property computed optional =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Typed.unTypedTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm optional)}]}))
-- | DSL accessor for the computed field of hydra.typeScript.syntax.MemberExpression
memberExpressionComputed :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Bool
memberExpressionComputed x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
        Core.projectionFieldName = (Core.Name "computed")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the object field of hydra.typeScript.syntax.MemberExpression
memberExpressionObject :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Syntax.Expression
memberExpressionObject x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
        Core.projectionFieldName = (Core.Name "object")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the optional field of hydra.typeScript.syntax.MemberExpression
memberExpressionOptional :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Bool
memberExpressionOptional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
        Core.projectionFieldName = (Core.Name "optional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the property field of hydra.typeScript.syntax.MemberExpression
memberExpressionProperty :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Syntax.Expression
memberExpressionProperty x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
        Core.projectionFieldName = (Core.Name "property")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the computed field of hydra.typeScript.syntax.MemberExpression
memberExpressionWithComputed :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MemberExpression
memberExpressionWithComputed original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the object field of hydra.typeScript.syntax.MemberExpression
memberExpressionWithObject :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MemberExpression
memberExpressionWithObject original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the optional field of hydra.typeScript.syntax.MemberExpression
memberExpressionWithOptional :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MemberExpression
memberExpressionWithOptional original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the property field of hydra.typeScript.syntax.MemberExpression
memberExpressionWithProperty :: Typed.TypedTerm Syntax.MemberExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MemberExpression
memberExpressionWithProperty original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "object")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MemberExpression"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.MethodDefinition
methodDefinition :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Syntax.MethodKind -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinition key value kind computed static =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Typed.unTypedTerm static)}]}))
-- | DSL accessor for the computed field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionComputed :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Bool
methodDefinitionComputed x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
        Core.projectionFieldName = (Core.Name "computed")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the key field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionKey :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.Expression
methodDefinitionKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionKind :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.MethodKind
methodDefinitionKind x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the static field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionStatic :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Bool
methodDefinitionStatic x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
        Core.projectionFieldName = (Core.Name "static")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionValue :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.FunctionExpression
methodDefinitionValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the computed field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionWithComputed :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinitionWithComputed original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "static")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the key field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionWithKey :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinitionWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "static")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the kind field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionWithKind :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.MethodKind -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinitionWithKind original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "static")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the static field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionWithStatic :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinitionWithStatic original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.typeScript.syntax.MethodDefinition
methodDefinitionWithValue :: Typed.TypedTerm Syntax.MethodDefinition -> Typed.TypedTerm Syntax.FunctionExpression -> Typed.TypedTerm Syntax.MethodDefinition
methodDefinitionWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "static"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodDefinition"),
              Core.projectionFieldName = (Core.Name "static")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the constructor variant of hydra.typeScript.syntax.MethodKind
methodKindConstructor :: Typed.TypedTerm Syntax.MethodKind
methodKindConstructor =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "constructor"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the get variant of hydra.typeScript.syntax.MethodKind
methodKindGet :: Typed.TypedTerm Syntax.MethodKind
methodKindGet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "get"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the method variant of hydra.typeScript.syntax.MethodKind
methodKindMethod :: Typed.TypedTerm Syntax.MethodKind
methodKindMethod =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "method"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.typeScript.syntax.MethodKind
methodKindSet :: Typed.TypedTerm Syntax.MethodKind
methodKindSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.MethodKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the export variant of hydra.typeScript.syntax.ModuleItem
moduleItemExport :: Typed.TypedTerm Syntax.ExportDeclaration -> Typed.TypedTerm Syntax.ModuleItem
moduleItemExport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "export"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the import variant of hydra.typeScript.syntax.ModuleItem
moduleItemImport :: Typed.TypedTerm Syntax.ImportDeclaration -> Typed.TypedTerm Syntax.ModuleItem
moduleItemImport x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "import"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the interface variant of hydra.typeScript.syntax.ModuleItem
moduleItemInterface :: Typed.TypedTerm Syntax.InterfaceDeclaration -> Typed.TypedTerm Syntax.ModuleItem
moduleItemInterface x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "interface"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the statement variant of hydra.typeScript.syntax.ModuleItem
moduleItemStatement :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.ModuleItem
moduleItemStatement x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "statement"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typeAlias variant of hydra.typeScript.syntax.ModuleItem
moduleItemTypeAlias :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm Syntax.ModuleItem
moduleItemTypeAlias x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeAlias"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ModuleItemWithComments
moduleItemWithComments :: Typed.TypedTerm Syntax.ModuleItem -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.ModuleItemWithComments
moduleItemWithComments body comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.ModuleItemWithComments
moduleItemWithCommentsBody :: Typed.TypedTerm Syntax.ModuleItemWithComments -> Typed.TypedTerm Syntax.ModuleItem
moduleItemWithCommentsBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the comments field of hydra.typeScript.syntax.ModuleItemWithComments
moduleItemWithCommentsComments :: Typed.TypedTerm Syntax.ModuleItemWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment)
moduleItemWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.ModuleItemWithComments
moduleItemWithCommentsWithBody :: Typed.TypedTerm Syntax.ModuleItemWithComments -> Typed.TypedTerm Syntax.ModuleItem -> Typed.TypedTerm Syntax.ModuleItemWithComments
moduleItemWithCommentsWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the comments field of hydra.typeScript.syntax.ModuleItemWithComments
moduleItemWithCommentsWithComments :: Typed.TypedTerm Syntax.ModuleItemWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.ModuleItemWithComments
moduleItemWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ModuleItemWithComments"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.NamedExport
namedExport :: Typed.TypedTerm [Syntax.ExportSpecifier] -> Typed.TypedTerm (Maybe Syntax.StringLiteral) -> Typed.TypedTerm Syntax.NamedExport
namedExport specifiers source =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Typed.unTypedTerm specifiers)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)}]}))
-- | DSL accessor for the source field of hydra.typeScript.syntax.NamedExport
namedExportSource :: Typed.TypedTerm Syntax.NamedExport -> Typed.TypedTerm (Maybe Syntax.StringLiteral)
namedExportSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the specifiers field of hydra.typeScript.syntax.NamedExport
namedExportSpecifiers :: Typed.TypedTerm Syntax.NamedExport -> Typed.TypedTerm [Syntax.ExportSpecifier]
namedExportSpecifiers x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
        Core.projectionFieldName = (Core.Name "specifiers")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the source field of hydra.typeScript.syntax.NamedExport
namedExportWithSource :: Typed.TypedTerm Syntax.NamedExport -> Typed.TypedTerm (Maybe Syntax.StringLiteral) -> Typed.TypedTerm Syntax.NamedExport
namedExportWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
              Core.projectionFieldName = (Core.Name "specifiers")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the specifiers field of hydra.typeScript.syntax.NamedExport
namedExportWithSpecifiers :: Typed.TypedTerm Syntax.NamedExport -> Typed.TypedTerm [Syntax.ExportSpecifier] -> Typed.TypedTerm Syntax.NamedExport
namedExportWithSpecifiers original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "specifiers"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.NamedExport"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the float variant of hydra.typeScript.syntax.NumericLiteral
numericLiteralFloat :: Typed.TypedTerm Double -> Typed.TypedTerm Syntax.NumericLiteral
numericLiteralFloat x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the integer variant of hydra.typeScript.syntax.NumericLiteral
numericLiteralInteger :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Syntax.NumericLiteral
numericLiteralInteger x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.NumericLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.ObjectPattern
objectPattern :: Typed.TypedTerm [Syntax.ObjectPatternProperty] -> Typed.TypedTerm Syntax.ObjectPattern
objectPattern properties =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ObjectPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm properties)}]}))
-- | DSL accessor for the properties field of hydra.typeScript.syntax.ObjectPattern
objectPatternProperties :: Typed.TypedTerm Syntax.ObjectPattern -> Typed.TypedTerm [Syntax.ObjectPatternProperty]
objectPatternProperties x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ObjectPattern"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the property variant of hydra.typeScript.syntax.ObjectPatternProperty
objectPatternPropertyProperty :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.ObjectPatternProperty
objectPatternPropertyProperty x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ObjectPatternProperty"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rest variant of hydra.typeScript.syntax.ObjectPatternProperty
objectPatternPropertyRest :: Typed.TypedTerm Syntax.RestElement -> Typed.TypedTerm Syntax.ObjectPatternProperty
objectPatternPropertyRest x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.ObjectPatternProperty"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL updater for the properties field of hydra.typeScript.syntax.ObjectPattern
objectPatternWithProperties :: Typed.TypedTerm Syntax.ObjectPattern -> Typed.TypedTerm [Syntax.ObjectPatternProperty] -> Typed.TypedTerm Syntax.ObjectPattern
objectPatternWithProperties original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ObjectPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.ParameterizedTypeExpression
parameterizedTypeExpression :: Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpression base arguments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Typed.unTypedTerm base)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm arguments)}]}))
-- | DSL accessor for the arguments field of hydra.typeScript.syntax.ParameterizedTypeExpression
parameterizedTypeExpressionArguments :: Typed.TypedTerm Syntax.ParameterizedTypeExpression -> Typed.TypedTerm [Syntax.TypeExpression]
parameterizedTypeExpressionArguments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the base field of hydra.typeScript.syntax.ParameterizedTypeExpression
parameterizedTypeExpressionBase :: Typed.TypedTerm Syntax.ParameterizedTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
parameterizedTypeExpressionBase x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
        Core.projectionFieldName = (Core.Name "base")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the arguments field of hydra.typeScript.syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithArguments :: Typed.TypedTerm Syntax.ParameterizedTypeExpression -> Typed.TypedTerm [Syntax.TypeExpression] -> Typed.TypedTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithArguments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
              Core.projectionFieldName = (Core.Name "base")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the base field of hydra.typeScript.syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithBase :: Typed.TypedTerm Syntax.ParameterizedTypeExpression -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.ParameterizedTypeExpression
parameterizedTypeExpressionWithBase original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.ParameterizedTypeExpression"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the array variant of hydra.typeScript.syntax.Pattern
patternArray :: Typed.TypedTerm Syntax.ArrayPattern -> Typed.TypedTerm Syntax.Pattern
patternArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the assignment variant of hydra.typeScript.syntax.Pattern
patternAssignment :: Typed.TypedTerm Syntax.AssignmentPattern -> Typed.TypedTerm Syntax.Pattern
patternAssignment x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "assignment"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the identifier variant of hydra.typeScript.syntax.Pattern
patternIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.Pattern
patternIdentifier x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the object variant of hydra.typeScript.syntax.Pattern
patternObject :: Typed.TypedTerm Syntax.ObjectPattern -> Typed.TypedTerm Syntax.Pattern
patternObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the rest variant of hydra.typeScript.syntax.Pattern
patternRest :: Typed.TypedTerm Syntax.RestElement -> Typed.TypedTerm Syntax.Pattern
patternRest x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rest"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the typed variant of hydra.typeScript.syntax.Pattern
patternTyped :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern
patternTyped x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Pattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typed"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.Program
program :: Typed.TypedTerm [Syntax.ModuleItem] -> Typed.TypedTerm Syntax.SourceType -> Typed.TypedTerm Syntax.Program
program body sourceType =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Typed.unTypedTerm sourceType)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.Program
programBody :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ModuleItem]
programBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the sourceType field of hydra.typeScript.syntax.Program
programSourceType :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm Syntax.SourceType
programSourceType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
        Core.projectionFieldName = (Core.Name "sourceType")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.Program
programWithBody :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm [Syntax.ModuleItem] -> Typed.TypedTerm Syntax.Program
programWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
              Core.projectionFieldName = (Core.Name "sourceType")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the sourceType field of hydra.typeScript.syntax.Program
programWithSourceType :: Typed.TypedTerm Syntax.Program -> Typed.TypedTerm Syntax.SourceType -> Typed.TypedTerm Syntax.Program
programWithSourceType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Program"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sourceType"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.Property
property :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.PropertyKind -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Property
property key value kind computed shorthand =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm computed)},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Typed.unTypedTerm shorthand)}]}))
-- | DSL accessor for the computed field of hydra.typeScript.syntax.Property
propertyComputed :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Bool
propertyComputed x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
        Core.projectionFieldName = (Core.Name "computed")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the key field of hydra.typeScript.syntax.Property
propertyKey :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.Expression
propertyKey x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.typeScript.syntax.Property
propertyKind :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.PropertyKind
propertyKind x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the get variant of hydra.typeScript.syntax.PropertyKind
propertyKindGet :: Typed.TypedTerm Syntax.PropertyKind
propertyKindGet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "get"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the init variant of hydra.typeScript.syntax.PropertyKind
propertyKindInit :: Typed.TypedTerm Syntax.PropertyKind
propertyKindInit =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "init"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the set variant of hydra.typeScript.syntax.PropertyKind
propertyKindSet :: Typed.TypedTerm Syntax.PropertyKind
propertyKindSet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertyKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the shorthand field of hydra.typeScript.syntax.Property
propertyShorthand :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Bool
propertyShorthand x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
        Core.projectionFieldName = (Core.Name "shorthand")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typeScript.syntax.PropertySignature
propertySignature :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.PropertySignature
propertySignature name type_ optional readonly comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm optional)},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Typed.unTypedTerm readonly)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the comments field of hydra.typeScript.syntax.PropertySignature
propertySignatureComments :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm (Maybe Syntax.DocumentationComment)
propertySignatureComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typeScript.syntax.PropertySignature
propertySignatureName :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Syntax.Identifier
propertySignatureName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the optional field of hydra.typeScript.syntax.PropertySignature
propertySignatureOptional :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Bool
propertySignatureOptional x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
        Core.projectionFieldName = (Core.Name "optional")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the readonly field of hydra.typeScript.syntax.PropertySignature
propertySignatureReadonly :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Bool
propertySignatureReadonly x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
        Core.projectionFieldName = (Core.Name "readonly")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typeScript.syntax.PropertySignature
propertySignatureType :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Syntax.TypeExpression
propertySignatureType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.typeScript.syntax.PropertySignature
propertySignatureWithComments :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.PropertySignature
propertySignatureWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "readonly")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typeScript.syntax.PropertySignature
propertySignatureWithName :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.PropertySignature
propertySignatureWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "readonly")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the optional field of hydra.typeScript.syntax.PropertySignature
propertySignatureWithOptional :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.PropertySignature
propertySignatureWithOptional original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "readonly")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the readonly field of hydra.typeScript.syntax.PropertySignature
propertySignatureWithReadonly :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.PropertySignature
propertySignatureWithReadonly original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typeScript.syntax.PropertySignature
propertySignatureWithType :: Typed.TypedTerm Syntax.PropertySignature -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.PropertySignature
propertySignatureWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "readonly"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "readonly")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.PropertySignature"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the value field of hydra.typeScript.syntax.Property
propertyValue :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.Expression
propertyValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the computed field of hydra.typeScript.syntax.Property
propertyWithComputed :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Property
propertyWithComputed original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "shorthand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the key field of hydra.typeScript.syntax.Property
propertyWithKey :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Property
propertyWithKey original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "shorthand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the kind field of hydra.typeScript.syntax.Property
propertyWithKind :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.PropertyKind -> Typed.TypedTerm Syntax.Property
propertyWithKind original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "shorthand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the shorthand field of hydra.typeScript.syntax.Property
propertyWithShorthand :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.Property
propertyWithShorthand original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.typeScript.syntax.Property
propertyWithValue :: Typed.TypedTerm Syntax.Property -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Property
propertyWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "computed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "computed")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "shorthand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.Property"),
              Core.projectionFieldName = (Core.Name "shorthand")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typeScript.syntax.RestElement wrapper
restElement :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.RestElement
restElement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.RestElement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the module variant of hydra.typeScript.syntax.SourceType
sourceTypeModule :: Typed.TypedTerm Syntax.SourceType
sourceTypeModule =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.SourceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the script variant of hydra.typeScript.syntax.SourceType
sourceTypeScript :: Typed.TypedTerm Syntax.SourceType
sourceTypeScript =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.SourceType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "script"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for the hydra.typeScript.syntax.SpreadElement wrapper
spreadElement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SpreadElement
spreadElement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.SpreadElement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the block variant of hydra.typeScript.syntax.Statement
statementBlock :: Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.Statement
statementBlock x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "block"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the break variant of hydra.typeScript.syntax.Statement
statementBreak :: Typed.TypedTerm Syntax.BreakStatement -> Typed.TypedTerm Syntax.Statement
statementBreak x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "break"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the classDeclaration variant of hydra.typeScript.syntax.Statement
statementClassDeclaration :: Typed.TypedTerm Syntax.ClassDeclaration -> Typed.TypedTerm Syntax.Statement
statementClassDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "classDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the continue variant of hydra.typeScript.syntax.Statement
statementContinue :: Typed.TypedTerm Syntax.ContinueStatement -> Typed.TypedTerm Syntax.Statement
statementContinue x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "continue"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the debugger variant of hydra.typeScript.syntax.Statement
statementDebugger :: Typed.TypedTerm Syntax.Statement
statementDebugger =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "debugger"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the doWhile variant of hydra.typeScript.syntax.Statement
statementDoWhile :: Typed.TypedTerm Syntax.DoWhileStatement -> Typed.TypedTerm Syntax.Statement
statementDoWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "doWhile"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the empty variant of hydra.typeScript.syntax.Statement
statementEmpty :: Typed.TypedTerm Syntax.Statement
statementEmpty =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "empty"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the expression variant of hydra.typeScript.syntax.Statement
statementExpression :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement
statementExpression x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "expression"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the for variant of hydra.typeScript.syntax.Statement
statementFor :: Typed.TypedTerm Syntax.ForStatement -> Typed.TypedTerm Syntax.Statement
statementFor x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "for"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the forIn variant of hydra.typeScript.syntax.Statement
statementForIn :: Typed.TypedTerm Syntax.ForInStatement -> Typed.TypedTerm Syntax.Statement
statementForIn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forIn"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the forOf variant of hydra.typeScript.syntax.Statement
statementForOf :: Typed.TypedTerm Syntax.ForOfStatement -> Typed.TypedTerm Syntax.Statement
statementForOf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "forOf"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the functionDeclaration variant of hydra.typeScript.syntax.Statement
statementFunctionDeclaration :: Typed.TypedTerm Syntax.FunctionDeclaration -> Typed.TypedTerm Syntax.Statement
statementFunctionDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the if variant of hydra.typeScript.syntax.Statement
statementIf :: Typed.TypedTerm Syntax.IfStatement -> Typed.TypedTerm Syntax.Statement
statementIf x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "if"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the labeled variant of hydra.typeScript.syntax.Statement
statementLabeled :: Typed.TypedTerm Syntax.LabeledStatement -> Typed.TypedTerm Syntax.Statement
statementLabeled x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labeled"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the return variant of hydra.typeScript.syntax.Statement
statementReturn :: Typed.TypedTerm Syntax.ReturnStatement -> Typed.TypedTerm Syntax.Statement
statementReturn x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "return"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the switch variant of hydra.typeScript.syntax.Statement
statementSwitch :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.Statement
statementSwitch x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "switch"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the throw variant of hydra.typeScript.syntax.Statement
statementThrow :: Typed.TypedTerm Syntax.ThrowStatement -> Typed.TypedTerm Syntax.Statement
statementThrow x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "throw"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the try variant of hydra.typeScript.syntax.Statement
statementTry :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm Syntax.Statement
statementTry x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "try"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the variableDeclaration variant of hydra.typeScript.syntax.Statement
statementVariableDeclaration :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm Syntax.Statement
statementVariableDeclaration x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableDeclaration"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the while variant of hydra.typeScript.syntax.Statement
statementWhile :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement
statementWhile x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.Statement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "while"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.typeScript.syntax.StatementWithComments
statementWithComments :: Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.StatementWithComments
statementWithComments body comments =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.StatementWithComments
statementWithCommentsBody :: Typed.TypedTerm Syntax.StatementWithComments -> Typed.TypedTerm Syntax.Statement
statementWithCommentsBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the comments field of hydra.typeScript.syntax.StatementWithComments
statementWithCommentsComments :: Typed.TypedTerm Syntax.StatementWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment)
statementWithCommentsComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.StatementWithComments
statementWithCommentsWithBody :: Typed.TypedTerm Syntax.StatementWithComments -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.StatementWithComments
statementWithCommentsWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the comments field of hydra.typeScript.syntax.StatementWithComments
statementWithCommentsWithComments :: Typed.TypedTerm Syntax.StatementWithComments -> Typed.TypedTerm (Maybe Syntax.DocumentationComment) -> Typed.TypedTerm Syntax.StatementWithComments
statementWithCommentsWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StatementWithComments"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.typeScript.syntax.StringLiteral
stringLiteral :: Typed.TypedTerm String -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.StringLiteral
stringLiteral value singleQuote =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Typed.unTypedTerm singleQuote)}]}))
-- | DSL accessor for the singleQuote field of hydra.typeScript.syntax.StringLiteral
stringLiteralSingleQuote :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Bool
stringLiteralSingleQuote x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
        Core.projectionFieldName = (Core.Name "singleQuote")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.typeScript.syntax.StringLiteral
stringLiteralValue :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm String
stringLiteralValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the singleQuote field of hydra.typeScript.syntax.StringLiteral
stringLiteralWithSingleQuote :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.StringLiteral
stringLiteralWithSingleQuote original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.typeScript.syntax.StringLiteral
stringLiteralWithValue :: Typed.TypedTerm Syntax.StringLiteral -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.StringLiteral
stringLiteralWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "singleQuote"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.StringLiteral"),
              Core.projectionFieldName = (Core.Name "singleQuote")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.SwitchCase
switchCase :: Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.SwitchCase
switchCase test consequent =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm consequent)}]}))
-- | DSL accessor for the consequent field of hydra.typeScript.syntax.SwitchCase
switchCaseConsequent :: Typed.TypedTerm Syntax.SwitchCase -> Typed.TypedTerm [Syntax.Statement]
switchCaseConsequent x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
        Core.projectionFieldName = (Core.Name "consequent")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.SwitchCase
switchCaseTest :: Typed.TypedTerm Syntax.SwitchCase -> Typed.TypedTerm (Maybe Syntax.Expression)
switchCaseTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the consequent field of hydra.typeScript.syntax.SwitchCase
switchCaseWithConsequent :: Typed.TypedTerm Syntax.SwitchCase -> Typed.TypedTerm [Syntax.Statement] -> Typed.TypedTerm Syntax.SwitchCase
switchCaseWithConsequent original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.SwitchCase
switchCaseWithTest :: Typed.TypedTerm Syntax.SwitchCase -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.SwitchCase
switchCaseWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "consequent"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchCase"),
              Core.projectionFieldName = (Core.Name "consequent")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.SwitchStatement
switchStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm [Syntax.SwitchCase] -> Typed.TypedTerm Syntax.SwitchStatement
switchStatement discriminant cases =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Typed.unTypedTerm discriminant)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm cases)}]}))
-- | DSL accessor for the cases field of hydra.typeScript.syntax.SwitchStatement
switchStatementCases :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm [Syntax.SwitchCase]
switchStatementCases x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "cases")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the discriminant field of hydra.typeScript.syntax.SwitchStatement
switchStatementDiscriminant :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.Expression
switchStatementDiscriminant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
        Core.projectionFieldName = (Core.Name "discriminant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the cases field of hydra.typeScript.syntax.SwitchStatement
switchStatementWithCases :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm [Syntax.SwitchCase] -> Typed.TypedTerm Syntax.SwitchStatement
switchStatementWithCases original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
              Core.projectionFieldName = (Core.Name "discriminant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the discriminant field of hydra.typeScript.syntax.SwitchStatement
switchStatementWithDiscriminant :: Typed.TypedTerm Syntax.SwitchStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.SwitchStatement
switchStatementWithDiscriminant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "discriminant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.SwitchStatement"),
              Core.projectionFieldName = (Core.Name "cases")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.TemplateElement
templateElement :: Typed.TypedTerm String -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.TemplateElement
templateElement value tail =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm tail)}]}))
-- | DSL accessor for the tail field of hydra.typeScript.syntax.TemplateElement
templateElementTail :: Typed.TypedTerm Syntax.TemplateElement -> Typed.TypedTerm Bool
templateElementTail x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
        Core.projectionFieldName = (Core.Name "tail")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the value field of hydra.typeScript.syntax.TemplateElement
templateElementValue :: Typed.TypedTerm Syntax.TemplateElement -> Typed.TypedTerm String
templateElementValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the tail field of hydra.typeScript.syntax.TemplateElement
templateElementWithTail :: Typed.TypedTerm Syntax.TemplateElement -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.TemplateElement
templateElementWithTail original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the value field of hydra.typeScript.syntax.TemplateElement
templateElementWithValue :: Typed.TypedTerm Syntax.TemplateElement -> Typed.TypedTerm String -> Typed.TypedTerm Syntax.TemplateElement
templateElementWithValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateElement"),
              Core.projectionFieldName = (Core.Name "tail")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.TemplateLiteral
templateLiteral :: Typed.TypedTerm [Syntax.TemplateElement] -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.TemplateLiteral
templateLiteral quasis expressions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Typed.unTypedTerm quasis)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm expressions)}]}))
-- | DSL accessor for the expressions field of hydra.typeScript.syntax.TemplateLiteral
templateLiteralExpressions :: Typed.TypedTerm Syntax.TemplateLiteral -> Typed.TypedTerm [Syntax.Expression]
templateLiteralExpressions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the quasis field of hydra.typeScript.syntax.TemplateLiteral
templateLiteralQuasis :: Typed.TypedTerm Syntax.TemplateLiteral -> Typed.TypedTerm [Syntax.TemplateElement]
templateLiteralQuasis x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
        Core.projectionFieldName = (Core.Name "quasis")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the expressions field of hydra.typeScript.syntax.TemplateLiteral
templateLiteralWithExpressions :: Typed.TypedTerm Syntax.TemplateLiteral -> Typed.TypedTerm [Syntax.Expression] -> Typed.TypedTerm Syntax.TemplateLiteral
templateLiteralWithExpressions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
              Core.projectionFieldName = (Core.Name "quasis")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the quasis field of hydra.typeScript.syntax.TemplateLiteral
templateLiteralWithQuasis :: Typed.TypedTerm Syntax.TemplateLiteral -> Typed.TypedTerm [Syntax.TemplateElement] -> Typed.TypedTerm Syntax.TemplateLiteral
templateLiteralWithQuasis original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "quasis"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TemplateLiteral"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typeScript.syntax.ThrowStatement wrapper
throwStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.ThrowStatement
throwStatement x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.ThrowStatement"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typeScript.syntax.TryStatement
tryStatement :: Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm (Maybe Syntax.CatchClause) -> Typed.TypedTerm (Maybe Syntax.BlockStatement) -> Typed.TypedTerm Syntax.TryStatement
tryStatement block handler finalizer =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm block)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Typed.unTypedTerm handler)},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Typed.unTypedTerm finalizer)}]}))
-- | DSL accessor for the block field of hydra.typeScript.syntax.TryStatement
tryStatementBlock :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm Syntax.BlockStatement
tryStatementBlock x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
        Core.projectionFieldName = (Core.Name "block")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the finalizer field of hydra.typeScript.syntax.TryStatement
tryStatementFinalizer :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm (Maybe Syntax.BlockStatement)
tryStatementFinalizer x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
        Core.projectionFieldName = (Core.Name "finalizer")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the handler field of hydra.typeScript.syntax.TryStatement
tryStatementHandler :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm (Maybe Syntax.CatchClause)
tryStatementHandler x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
        Core.projectionFieldName = (Core.Name "handler")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the block field of hydra.typeScript.syntax.TryStatement
tryStatementWithBlock :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm Syntax.BlockStatement -> Typed.TypedTerm Syntax.TryStatement
tryStatementWithBlock original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "handler")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "finalizer")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the finalizer field of hydra.typeScript.syntax.TryStatement
tryStatementWithFinalizer :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm (Maybe Syntax.BlockStatement) -> Typed.TypedTerm Syntax.TryStatement
tryStatementWithFinalizer original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "handler")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the handler field of hydra.typeScript.syntax.TryStatement
tryStatementWithHandler :: Typed.TypedTerm Syntax.TryStatement -> Typed.TypedTerm (Maybe Syntax.CatchClause) -> Typed.TypedTerm Syntax.TryStatement
tryStatementWithHandler original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "block"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "block")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "handler"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "finalizer"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TryStatement"),
              Core.projectionFieldName = (Core.Name "finalizer")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclaration :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypeAliasDeclaration
typeAliasDeclaration name typeParameters type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm typeParameters)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the name field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationName :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm Syntax.Identifier
typeAliasDeclarationName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationType :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm Syntax.TypeExpression
typeAliasDeclarationType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeParameters field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationTypeParameters :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm [Syntax.TypeParameter]
typeAliasDeclarationTypeParameters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
        Core.projectionFieldName = (Core.Name "typeParameters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationWithName :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeAliasDeclaration
typeAliasDeclarationWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationWithType :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypeAliasDeclaration
typeAliasDeclarationWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "typeParameters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the typeParameters field of hydra.typeScript.syntax.TypeAliasDeclaration
typeAliasDeclarationWithTypeParameters :: Typed.TypedTerm Syntax.TypeAliasDeclaration -> Typed.TypedTerm [Syntax.TypeParameter] -> Typed.TypedTerm Syntax.TypeAliasDeclaration
typeAliasDeclarationWithTypeParameters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeParameters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeAliasDeclaration"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for the hydra.typeScript.syntax.TypeAnnotation wrapper
typeAnnotation :: Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypeAnnotation
typeAnnotation x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typeScript.syntax.TypeAnnotation"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the any variant of hydra.typeScript.syntax.TypeExpression
typeExpressionAny :: Typed.TypedTerm Syntax.TypeExpression
typeExpressionAny =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the array variant of hydra.typeScript.syntax.TypeExpression
typeExpressionArray :: Typed.TypedTerm Syntax.ArrayTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionArray x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "array"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the function variant of hydra.typeScript.syntax.TypeExpression
typeExpressionFunction :: Typed.TypedTerm Syntax.FunctionTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionFunction x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "function"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the identifier variant of hydra.typeScript.syntax.TypeExpression
typeExpressionIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionIdentifier x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "identifier"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the intersection variant of hydra.typeScript.syntax.TypeExpression
typeExpressionIntersection :: Typed.TypedTerm Syntax.IntersectionTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionIntersection x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "intersection"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the literal variant of hydra.typeScript.syntax.TypeExpression
typeExpressionLiteral :: Typed.TypedTerm Syntax.Literal -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionLiteral x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the never variant of hydra.typeScript.syntax.TypeExpression
typeExpressionNever :: Typed.TypedTerm Syntax.TypeExpression
typeExpressionNever =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "never"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the object variant of hydra.typeScript.syntax.TypeExpression
typeExpressionObject :: Typed.TypedTerm Syntax.ObjectTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionObject x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "object"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the optional variant of hydra.typeScript.syntax.TypeExpression
typeExpressionOptional :: Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionOptional x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the parameterized variant of hydra.typeScript.syntax.TypeExpression
typeExpressionParameterized :: Typed.TypedTerm Syntax.ParameterizedTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionParameterized x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameterized"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the readonly variant of hydra.typeScript.syntax.TypeExpression
typeExpressionReadonly :: Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionReadonly x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "readonly"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the tuple variant of hydra.typeScript.syntax.TypeExpression
typeExpressionTuple :: Typed.TypedTerm Syntax.TupleTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionTuple x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "tuple"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the union variant of hydra.typeScript.syntax.TypeExpression
typeExpressionUnion :: Typed.TypedTerm Syntax.UnionTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
typeExpressionUnion x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the unknown variant of hydra.typeScript.syntax.TypeExpression
typeExpressionUnknown :: Typed.TypedTerm Syntax.TypeExpression
typeExpressionUnknown =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unknown"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the void variant of hydra.typeScript.syntax.TypeExpression
typeExpressionVoid :: Typed.TypedTerm Syntax.TypeExpression
typeExpressionVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.TypeParameter
typeParameter :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm Syntax.TypeParameter
typeParameter name constraint default_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Typed.unTypedTerm constraint)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm default_)}]}))
-- | DSL accessor for the constraint field of hydra.typeScript.syntax.TypeParameter
typeParameterConstraint :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeExpression)
typeParameterConstraint x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "constraint")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the default field of hydra.typeScript.syntax.TypeParameter
typeParameterDefault :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeExpression)
typeParameterDefault x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "default")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.typeScript.syntax.TypeParameter
typeParameterName :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm Syntax.Identifier
typeParameterName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constraint field of hydra.typeScript.syntax.TypeParameter
typeParameterWithConstraint :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm Syntax.TypeParameter
typeParameterWithConstraint original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the default field of hydra.typeScript.syntax.TypeParameter
typeParameterWithDefault :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm (Maybe Syntax.TypeExpression) -> Typed.TypedTerm Syntax.TypeParameter
typeParameterWithDefault original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "constraint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.typeScript.syntax.TypeParameter
typeParameterWithName :: Typed.TypedTerm Syntax.TypeParameter -> Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm Syntax.TypeParameter
typeParameterWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraint"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "constraint")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "default"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypeParameter"),
              Core.projectionFieldName = (Core.Name "default")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.TypedPattern
typedPattern :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypedPattern
typedPattern pattern type_ =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)}]}))
-- | DSL accessor for the pattern field of hydra.typeScript.syntax.TypedPattern
typedPatternPattern :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern
typedPatternPattern x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.typeScript.syntax.TypedPattern
typedPatternType :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.TypeExpression
typedPatternType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the pattern field of hydra.typeScript.syntax.TypedPattern
typedPatternWithPattern :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.TypedPattern
typedPatternWithPattern original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.typeScript.syntax.TypedPattern
typedPatternWithType :: Typed.TypedTerm Syntax.TypedPattern -> Typed.TypedTerm Syntax.TypeExpression -> Typed.TypedTerm Syntax.TypedPattern
typedPatternWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.TypedPattern"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.typeScript.syntax.ArrayTypeExpression
unArrayTypeExpression :: Typed.TypedTerm Syntax.ArrayTypeExpression -> Typed.TypedTerm Syntax.TypeExpression
unArrayTypeExpression x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.ArrayTypeExpression")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.Identifier
unIdentifier :: Typed.TypedTerm Syntax.Identifier -> Typed.TypedTerm String
unIdentifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.Identifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.ImportDefaultSpecifier
unImportDefaultSpecifier :: Typed.TypedTerm Syntax.ImportDefaultSpecifier -> Typed.TypedTerm Syntax.Identifier
unImportDefaultSpecifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.ImportDefaultSpecifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.ImportNamespaceSpecifier
unImportNamespaceSpecifier :: Typed.TypedTerm Syntax.ImportNamespaceSpecifier -> Typed.TypedTerm Syntax.Identifier
unImportNamespaceSpecifier x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.ImportNamespaceSpecifier")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.RestElement
unRestElement :: Typed.TypedTerm Syntax.RestElement -> Typed.TypedTerm Syntax.Pattern
unRestElement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.RestElement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.SpreadElement
unSpreadElement :: Typed.TypedTerm Syntax.SpreadElement -> Typed.TypedTerm Syntax.Expression
unSpreadElement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.SpreadElement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.ThrowStatement
unThrowStatement :: Typed.TypedTerm Syntax.ThrowStatement -> Typed.TypedTerm Syntax.Expression
unThrowStatement x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.ThrowStatement")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.typeScript.syntax.TypeAnnotation
unTypeAnnotation :: Typed.TypedTerm Syntax.TypeAnnotation -> Typed.TypedTerm Syntax.TypeExpression
unTypeAnnotation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.typeScript.syntax.TypeAnnotation")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.typeScript.syntax.UnaryExpression
unaryExpression :: Typed.TypedTerm Syntax.UnaryOperator -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpression operator argument prefix =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm argument)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm prefix)}]}))
-- | DSL accessor for the argument field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionArgument :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.Expression
unaryExpressionArgument x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the operator field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionOperator :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryOperator
unaryExpressionOperator x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the prefix field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionPrefix :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Bool
unaryExpressionPrefix x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
        Core.projectionFieldName = (Core.Name "prefix")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the argument field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionWithArgument :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionWithArgument original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the operator field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionWithOperator :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Syntax.UnaryOperator -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionWithOperator original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "prefix")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the prefix field of hydra.typeScript.syntax.UnaryExpression
unaryExpressionWithPrefix :: Typed.TypedTerm Syntax.UnaryExpression -> Typed.TypedTerm Bool -> Typed.TypedTerm Syntax.UnaryExpression
unaryExpressionWithPrefix original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the bitwiseNot variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorBitwiseNot :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorBitwiseNot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bitwiseNot"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the decrement variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorDecrement :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorDecrement =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decrement"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the delete variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorDelete :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorDelete =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delete"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the increment variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorIncrement :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorIncrement =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "increment"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the negate variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorNegate :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorNegate =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the not variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorNot :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorNot =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the plus variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorPlus :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorPlus =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the typeof variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorTypeof :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorTypeof =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeof"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the void variant of hydra.typeScript.syntax.UnaryOperator
unaryOperatorVoid :: Typed.TypedTerm Syntax.UnaryOperator
unaryOperatorVoid =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "void"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.VariableDeclaration
variableDeclaration :: Typed.TypedTerm Syntax.VariableKind -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.VariableDeclaration
variableDeclaration kind declarations =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm declarations)}]}))
-- | DSL accessor for the declarations field of hydra.typeScript.syntax.VariableDeclaration
variableDeclarationDeclarations :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator]
variableDeclarationDeclarations x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
        Core.projectionFieldName = (Core.Name "declarations")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the kind field of hydra.typeScript.syntax.VariableDeclaration
variableDeclarationKind :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm Syntax.VariableKind
variableDeclarationKind x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
        Core.projectionFieldName = (Core.Name "kind")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the declarations field of hydra.typeScript.syntax.VariableDeclaration
variableDeclarationWithDeclarations :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm [Syntax.VariableDeclarator] -> Typed.TypedTerm Syntax.VariableDeclaration
variableDeclarationWithDeclarations original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
              Core.projectionFieldName = (Core.Name "kind")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the kind field of hydra.typeScript.syntax.VariableDeclaration
variableDeclarationWithKind :: Typed.TypedTerm Syntax.VariableDeclaration -> Typed.TypedTerm Syntax.VariableKind -> Typed.TypedTerm Syntax.VariableDeclaration
variableDeclarationWithKind original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "declarations"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclaration"),
              Core.projectionFieldName = (Core.Name "declarations")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.typeScript.syntax.VariableDeclarator
variableDeclarator :: Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.VariableDeclarator
variableDeclarator id init =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm init)}]}))
-- | DSL accessor for the id field of hydra.typeScript.syntax.VariableDeclarator
variableDeclaratorId :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm Syntax.Pattern
variableDeclaratorId x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the init field of hydra.typeScript.syntax.VariableDeclarator
variableDeclaratorInit :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm (Maybe Syntax.Expression)
variableDeclaratorInit x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
        Core.projectionFieldName = (Core.Name "init")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the id field of hydra.typeScript.syntax.VariableDeclarator
variableDeclaratorWithId :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm Syntax.Pattern -> Typed.TypedTerm Syntax.VariableDeclarator
variableDeclaratorWithId original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
              Core.projectionFieldName = (Core.Name "init")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the init field of hydra.typeScript.syntax.VariableDeclarator
variableDeclaratorWithInit :: Typed.TypedTerm Syntax.VariableDeclarator -> Typed.TypedTerm (Maybe Syntax.Expression) -> Typed.TypedTerm Syntax.VariableDeclarator
variableDeclaratorWithInit original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableDeclarator"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "init"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL injection for the const variant of hydra.typeScript.syntax.VariableKind
variableKindConst :: Typed.TypedTerm Syntax.VariableKind
variableKindConst =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "const"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the let variant of hydra.typeScript.syntax.VariableKind
variableKindLet :: Typed.TypedTerm Syntax.VariableKind
variableKindLet =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the var variant of hydra.typeScript.syntax.VariableKind
variableKindVar :: Typed.TypedTerm Syntax.VariableKind
variableKindVar =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.typeScript.syntax.VariableKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "var"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL constructor for hydra.typeScript.syntax.WhileStatement
whileStatement :: Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.WhileStatement
whileStatement test body =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm test)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm body)}]}))
-- | DSL accessor for the body field of hydra.typeScript.syntax.WhileStatement
whileStatementBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement
whileStatementBody x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the test field of hydra.typeScript.syntax.WhileStatement
whileStatementTest :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Expression
whileStatementTest x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
        Core.projectionFieldName = (Core.Name "test")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the body field of hydra.typeScript.syntax.WhileStatement
whileStatementWithBody :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Statement -> Typed.TypedTerm Syntax.WhileStatement
whileStatementWithBody original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "test")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the test field of hydra.typeScript.syntax.WhileStatement
whileStatementWithTest :: Typed.TypedTerm Syntax.WhileStatement -> Typed.TypedTerm Syntax.Expression -> Typed.TypedTerm Syntax.WhileStatement
whileStatementWithTest original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "test"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.typeScript.syntax.WhileStatement"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
