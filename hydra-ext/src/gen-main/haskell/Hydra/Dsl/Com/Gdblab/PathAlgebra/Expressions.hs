-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for com.gdblab.pathAlgebra.expressions

module Hydra.Dsl.Com.Gdblab.PathAlgebra.Expressions where

import qualified Com.Gdblab.PathAlgebra.Expressions as Expressions
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

andCondition :: Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.AndCondition
andCondition left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

andConditionLeft :: Phantoms.TTerm Expressions.AndCondition -> Phantoms.TTerm Expressions.SelectionCondition
andConditionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andConditionRight :: Phantoms.TTerm Expressions.AndCondition -> Phantoms.TTerm Expressions.SelectionCondition
andConditionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

andConditionWithLeft :: Phantoms.TTerm Expressions.AndCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.AndCondition
andConditionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

andConditionWithRight :: Phantoms.TTerm Expressions.AndCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.AndCondition
andConditionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

baseExpressionPaths0 :: Phantoms.TTerm Expressions.GraphReference -> Phantoms.TTerm Expressions.BaseExpression
baseExpressionPaths0 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paths0"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

baseExpressionPaths1 :: Phantoms.TTerm Expressions.GraphReference -> Phantoms.TTerm Expressions.BaseExpression
baseExpressionPaths1 x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paths1"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

baseExpressionPathsStar :: Phantoms.TTerm Expressions.GraphReference -> Phantoms.TTerm Expressions.BaseExpression
baseExpressionPathsStar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pathsStar"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

comparisonOperatorEqual :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGreaterThan :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorGreaterThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGreaterThanOrEqual :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorGreaterThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLessThan :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorLessThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLessThanOrEqual :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorLessThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNotEqual :: Phantoms.TTerm Expressions.ComparisonOperator
comparisonOperatorNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

edgePropertyRef :: Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.EdgePropertyRef
edgePropertyRef element property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

edgePropertyRefElement :: Phantoms.TTerm Expressions.EdgePropertyRef -> Phantoms.TTerm Expressions.PathElement
edgePropertyRefElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
        Core.projectionField = (Core.Name "element")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgePropertyRefProperty :: Phantoms.TTerm Expressions.EdgePropertyRef -> Phantoms.TTerm String
edgePropertyRefProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgePropertyRefWithElement :: Phantoms.TTerm Expressions.EdgePropertyRef -> Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm Expressions.EdgePropertyRef
edgePropertyRefWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgePropertyRefWithProperty :: Phantoms.TTerm Expressions.EdgePropertyRef -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.EdgePropertyRef
edgePropertyRefWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
              Core.projectionField = (Core.Name "element")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

graphReference :: Phantoms.TTerm String -> Phantoms.TTerm Expressions.GraphReference
graphReference x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GraphReference"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

groupByCriterionLength :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionNone :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSource :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionSource =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceLength :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionSourceLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceTarget :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionSourceTarget =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTarget"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceTargetLength :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionSourceTargetLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTargetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionTarget :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionTarget =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionTargetLength :: Phantoms.TTerm Expressions.GroupByCriterion
groupByCriterionTargetLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByExpression :: Phantoms.TTerm Expressions.GroupByCriterion -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.GroupByExpression
groupByExpression criterion expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTTerm criterion)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

groupByExpressionCriterion :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.GroupByCriterion
groupByExpressionCriterion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
        Core.projectionField = (Core.Name "criterion")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

groupByExpressionExpression :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.PathExpression
groupByExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

groupByExpressionWithCriterion :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.GroupByCriterion -> Phantoms.TTerm Expressions.GroupByExpression
groupByExpressionWithCriterion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

groupByExpressionWithExpression :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.GroupByExpression
groupByExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
              Core.projectionField = (Core.Name "criterion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

joinExpression :: Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.JoinExpression
joinExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

joinExpressionLeft :: Phantoms.TTerm Expressions.JoinExpression -> Phantoms.TTerm Expressions.PathExpression
joinExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinExpressionRight :: Phantoms.TTerm Expressions.JoinExpression -> Phantoms.TTerm Expressions.PathExpression
joinExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinExpressionWithLeft :: Phantoms.TTerm Expressions.JoinExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.JoinExpression
joinExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

joinExpressionWithRight :: Phantoms.TTerm Expressions.JoinExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.JoinExpression
joinExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

labelCondition :: Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.LabelCondition
labelCondition target value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

labelConditionTarget :: Phantoms.TTerm Expressions.LabelCondition -> Phantoms.TTerm Expressions.PathElement
labelConditionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labelConditionValue :: Phantoms.TTerm Expressions.LabelCondition -> Phantoms.TTerm String
labelConditionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

labelConditionWithTarget :: Phantoms.TTerm Expressions.LabelCondition -> Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm Expressions.LabelCondition
labelConditionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

labelConditionWithValue :: Phantoms.TTerm Expressions.LabelCondition -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.LabelCondition
labelConditionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

lengthCondition :: Phantoms.TTerm Int -> Phantoms.TTerm Expressions.LengthCondition
lengthCondition length =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm length)}]}))

lengthConditionLength :: Phantoms.TTerm Expressions.LengthCondition -> Phantoms.TTerm Int
lengthConditionLength x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
        Core.projectionField = (Core.Name "length")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

lengthConditionWithLength :: Phantoms.TTerm Expressions.LengthCondition -> Phantoms.TTerm Int -> Phantoms.TTerm Expressions.LengthCondition
lengthConditionWithLength original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalValueBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Expressions.LiteralValue
literalValueBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalValueFloat :: Phantoms.TTerm Double -> Phantoms.TTerm Expressions.LiteralValue
literalValueFloat x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalValueInteger :: Phantoms.TTerm Int -> Phantoms.TTerm Expressions.LiteralValue
literalValueInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalValueString :: Phantoms.TTerm String -> Phantoms.TTerm Expressions.LiteralValue
literalValueString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nodePropertyRef :: Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.NodePropertyRef
nodePropertyRef element property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

nodePropertyRefElement :: Phantoms.TTerm Expressions.NodePropertyRef -> Phantoms.TTerm Expressions.PathElement
nodePropertyRefElement x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
        Core.projectionField = (Core.Name "element")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePropertyRefProperty :: Phantoms.TTerm Expressions.NodePropertyRef -> Phantoms.TTerm String
nodePropertyRefProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePropertyRefWithElement :: Phantoms.TTerm Expressions.NodePropertyRef -> Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm Expressions.NodePropertyRef
nodePropertyRefWithElement original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodePropertyRefWithProperty :: Phantoms.TTerm Expressions.NodePropertyRef -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.NodePropertyRef
nodePropertyRefWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
              Core.projectionField = (Core.Name "element")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notCondition :: Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.NotCondition
notCondition condition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)}]}))

notConditionCondition :: Phantoms.TTerm Expressions.NotCondition -> Phantoms.TTerm Expressions.SelectionCondition
notConditionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notConditionWithCondition :: Phantoms.TTerm Expressions.NotCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.NotCondition
notConditionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

orCondition :: Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.OrCondition
orCondition left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

orConditionLeft :: Phantoms.TTerm Expressions.OrCondition -> Phantoms.TTerm Expressions.SelectionCondition
orConditionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orConditionRight :: Phantoms.TTerm Expressions.OrCondition -> Phantoms.TTerm Expressions.SelectionCondition
orConditionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orConditionWithLeft :: Phantoms.TTerm Expressions.OrCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.OrCondition
orConditionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

orConditionWithRight :: Phantoms.TTerm Expressions.OrCondition -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.OrCondition
orConditionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

orderByCriterionGroup :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionGroup =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionGroupPath :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionGroupPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartition :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionPartition =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partition"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionGroup :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionPartitionGroup =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroup"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionGroupPath :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionPartitionGroupPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionPath :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionPartitionPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPath :: Phantoms.TTerm Expressions.OrderByCriterion
orderByCriterionPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

orderByExpression :: Phantoms.TTerm Expressions.OrderByCriterion -> Phantoms.TTerm Expressions.SolutionSpaceExpression -> Phantoms.TTerm Expressions.OrderByExpression
orderByExpression criterion expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTTerm criterion)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

orderByExpressionCriterion :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.OrderByCriterion
orderByExpressionCriterion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
        Core.projectionField = (Core.Name "criterion")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orderByExpressionExpression :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression
orderByExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

orderByExpressionWithCriterion :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.OrderByCriterion -> Phantoms.TTerm Expressions.OrderByExpression
orderByExpressionWithCriterion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

orderByExpressionWithExpression :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression -> Phantoms.TTerm Expressions.OrderByExpression
orderByExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
              Core.projectionField = (Core.Name "criterion")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pathElementEdge :: Phantoms.TTerm Int -> Phantoms.TTerm Expressions.PathElement
pathElementEdge x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathElementFirst :: Phantoms.TTerm Expressions.PathElement
pathElementFirst =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "first"),
        Core.fieldTerm = Core.TermUnit}}))

pathElementLast :: Phantoms.TTerm Expressions.PathElement
pathElementLast =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "last"),
        Core.fieldTerm = Core.TermUnit}}))

pathElementNode :: Phantoms.TTerm Int -> Phantoms.TTerm Expressions.PathElement
pathElementNode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionBase :: Phantoms.TTerm Expressions.BaseExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionBase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "base"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionGroupBy :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionGroupBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionJoin :: Phantoms.TTerm Expressions.JoinExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionJoin x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "join"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionOrderBy :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionOrderBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orderBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionProjection :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionProjection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionRecursive :: Phantoms.TTerm Expressions.RecursiveExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionRecursive x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recursive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionSelection :: Phantoms.TTerm Expressions.SelectionExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionSelection x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selection"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathExpressionUnion :: Phantoms.TTerm Expressions.UnionExpression -> Phantoms.TTerm Expressions.PathExpression
pathExpressionUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathPropertyRefEndNode :: Phantoms.TTerm Expressions.PathPropertyRef
pathPropertyRefEndNode =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endNode"),
        Core.fieldTerm = Core.TermUnit}}))

pathPropertyRefLength :: Phantoms.TTerm Expressions.PathPropertyRef
pathPropertyRefLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

pathPropertyRefStartNode :: Phantoms.TTerm Expressions.PathPropertyRef
pathPropertyRefStartNode =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startNode"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsAcyclic :: Phantoms.TTerm Expressions.PathSemantics
pathSemanticsAcyclic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "acyclic"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsShortest :: Phantoms.TTerm Expressions.PathSemantics
pathSemanticsShortest =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortest"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsSimple :: Phantoms.TTerm Expressions.PathSemantics
pathSemanticsSimple =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsTrail :: Phantoms.TTerm Expressions.PathSemantics
pathSemanticsTrail =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trail"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsWalk :: Phantoms.TTerm Expressions.PathSemantics
pathSemanticsWalk =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "walk"),
        Core.fieldTerm = Core.TermUnit}}))

projectionExpression :: Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.SolutionSpaceExpression -> Phantoms.TTerm Expressions.ProjectionExpression
projectionExpression partitions groups paths expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Phantoms.unTTerm partitions)},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTTerm groups)},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Phantoms.unTTerm paths)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

projectionExpressionExpression :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression
projectionExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionExpressionGroups :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec
projectionExpressionGroups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionField = (Core.Name "groups")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionExpressionPartitions :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec
projectionExpressionPartitions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionField = (Core.Name "partitions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionExpressionPaths :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec
projectionExpressionPaths x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionField = (Core.Name "paths")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionExpressionWithExpression :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression -> Phantoms.TTerm Expressions.ProjectionExpression
projectionExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "partitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "groups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "paths")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionExpressionWithGroups :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.ProjectionExpression
projectionExpressionWithGroups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "partitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "paths")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionExpressionWithPartitions :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.ProjectionExpression
projectionExpressionWithPartitions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "groups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "paths")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionExpressionWithPaths :: Phantoms.TTerm Expressions.ProjectionExpression -> Phantoms.TTerm Expressions.ProjectionSpec -> Phantoms.TTerm Expressions.ProjectionExpression
projectionExpressionWithPaths original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "partitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "groups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionSpecAll :: Phantoms.TTerm Expressions.ProjectionSpec
projectionSpecAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

projectionSpecLimited :: Phantoms.TTerm Int -> Phantoms.TTerm Expressions.ProjectionSpec
projectionSpecLimited x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyComparisonCondition :: Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.ComparisonOperator -> Phantoms.TTerm Expressions.LiteralValue -> Phantoms.TTerm Expressions.PropertyComparisonCondition
propertyComparisonCondition target property operator value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

propertyComparisonConditionOperator :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.ComparisonOperator
propertyComparisonConditionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyComparisonConditionProperty :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm String
propertyComparisonConditionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyComparisonConditionTarget :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.PathElement
propertyComparisonConditionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyComparisonConditionValue :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.LiteralValue
propertyComparisonConditionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyComparisonConditionWithOperator :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.ComparisonOperator -> Phantoms.TTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyComparisonConditionWithProperty :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyComparisonConditionWithTarget :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyComparisonConditionWithValue :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.LiteralValue -> Phantoms.TTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyCondition :: Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.LiteralValue -> Phantoms.TTerm Expressions.PropertyCondition
propertyCondition target property value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

propertyConditionProperty :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm String
propertyConditionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionField = (Core.Name "property")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyConditionTarget :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm Expressions.PathElement
propertyConditionTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionField = (Core.Name "target")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyConditionValue :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm Expressions.LiteralValue
propertyConditionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyConditionWithProperty :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm String -> Phantoms.TTerm Expressions.PropertyCondition
propertyConditionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyConditionWithTarget :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm Expressions.PathElement -> Phantoms.TTerm Expressions.PropertyCondition
propertyConditionWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyConditionWithValue :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm Expressions.LiteralValue -> Phantoms.TTerm Expressions.PropertyCondition
propertyConditionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "target")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionField = (Core.Name "property")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyExtraction :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Expressions.PropertySource -> Phantoms.TTerm Expressions.PropertyExtraction
propertyExtraction alias source =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)}]}))

propertyExtractionAlias :: Phantoms.TTerm Expressions.PropertyExtraction -> Phantoms.TTerm (Maybe String)
propertyExtractionAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
        Core.projectionField = (Core.Name "alias")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExtractionSource :: Phantoms.TTerm Expressions.PropertyExtraction -> Phantoms.TTerm Expressions.PropertySource
propertyExtractionSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
        Core.projectionField = (Core.Name "source")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExtractionWithAlias :: Phantoms.TTerm Expressions.PropertyExtraction -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Expressions.PropertyExtraction
propertyExtractionWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
              Core.projectionField = (Core.Name "source")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyExtractionWithSource :: Phantoms.TTerm Expressions.PropertyExtraction -> Phantoms.TTerm Expressions.PropertySource -> Phantoms.TTerm Expressions.PropertyExtraction
propertyExtractionWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
              Core.projectionField = (Core.Name "alias")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertySourceEdgeProperty :: Phantoms.TTerm Expressions.EdgePropertyRef -> Phantoms.TTerm Expressions.PropertySource
propertySourceEdgeProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edgeProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertySourceNodeProperty :: Phantoms.TTerm Expressions.NodePropertyRef -> Phantoms.TTerm Expressions.PropertySource
propertySourceNodeProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertySourcePathProperty :: Phantoms.TTerm Expressions.PathPropertyRef -> Phantoms.TTerm Expressions.PropertySource
propertySourcePathProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pathProperty"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryExpression :: Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm (Maybe Expressions.ResultProjection) -> Phantoms.TTerm Expressions.QueryExpression
queryExpression pathExpression resultProjection =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Phantoms.unTTerm pathExpression)},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Phantoms.unTTerm resultProjection)}]}))

queryExpressionPathExpression :: Phantoms.TTerm Expressions.QueryExpression -> Phantoms.TTerm Expressions.PathExpression
queryExpressionPathExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
        Core.projectionField = (Core.Name "pathExpression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

queryExpressionResultProjection :: Phantoms.TTerm Expressions.QueryExpression -> Phantoms.TTerm (Maybe Expressions.ResultProjection)
queryExpressionResultProjection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
        Core.projectionField = (Core.Name "resultProjection")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

queryExpressionWithPathExpression :: Phantoms.TTerm Expressions.QueryExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.QueryExpression
queryExpressionWithPathExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
              Core.projectionField = (Core.Name "resultProjection")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

queryExpressionWithResultProjection :: Phantoms.TTerm Expressions.QueryExpression -> Phantoms.TTerm (Maybe Expressions.ResultProjection) -> Phantoms.TTerm Expressions.QueryExpression
queryExpressionWithResultProjection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
              Core.projectionField = (Core.Name "pathExpression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recursiveExpression :: Phantoms.TTerm Expressions.PathSemantics -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.RecursiveExpression
recursiveExpression semantics expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Phantoms.unTTerm semantics)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

recursiveExpressionExpression :: Phantoms.TTerm Expressions.RecursiveExpression -> Phantoms.TTerm Expressions.PathExpression
recursiveExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recursiveExpressionSemantics :: Phantoms.TTerm Expressions.RecursiveExpression -> Phantoms.TTerm Expressions.PathSemantics
recursiveExpressionSemantics x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
        Core.projectionField = (Core.Name "semantics")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

recursiveExpressionWithExpression :: Phantoms.TTerm Expressions.RecursiveExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.RecursiveExpression
recursiveExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
              Core.projectionField = (Core.Name "semantics")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

recursiveExpressionWithSemantics :: Phantoms.TTerm Expressions.RecursiveExpression -> Phantoms.TTerm Expressions.PathSemantics -> Phantoms.TTerm Expressions.RecursiveExpression
recursiveExpressionWithSemantics original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

resultProjection :: Phantoms.TTerm [Expressions.PropertyExtraction] -> Phantoms.TTerm Expressions.ResultProjection
resultProjection projections =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projections"),
          Core.fieldTerm = (Phantoms.unTTerm projections)}]}))

resultProjectionProjections :: Phantoms.TTerm Expressions.ResultProjection -> Phantoms.TTerm [Expressions.PropertyExtraction]
resultProjectionProjections x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
        Core.projectionField = (Core.Name "projections")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

resultProjectionWithProjections :: Phantoms.TTerm Expressions.ResultProjection -> Phantoms.TTerm [Expressions.PropertyExtraction] -> Phantoms.TTerm Expressions.ResultProjection
resultProjectionWithProjections original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projections"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

selectionConditionAnd :: Phantoms.TTerm Expressions.AndCondition -> Phantoms.TTerm Expressions.SelectionCondition
selectionConditionAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionConditionNot :: Phantoms.TTerm Expressions.NotCondition -> Phantoms.TTerm Expressions.SelectionCondition
selectionConditionNot x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionConditionOr :: Phantoms.TTerm Expressions.OrCondition -> Phantoms.TTerm Expressions.SelectionCondition
selectionConditionOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionConditionSimple :: Phantoms.TTerm Expressions.SimpleCondition -> Phantoms.TTerm Expressions.SelectionCondition
selectionConditionSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectionExpression :: Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.SelectionExpression
selectionExpression condition expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

selectionExpressionCondition :: Phantoms.TTerm Expressions.SelectionExpression -> Phantoms.TTerm Expressions.SelectionCondition
selectionExpressionCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
        Core.projectionField = (Core.Name "condition")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectionExpressionExpression :: Phantoms.TTerm Expressions.SelectionExpression -> Phantoms.TTerm Expressions.PathExpression
selectionExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
        Core.projectionField = (Core.Name "expression")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectionExpressionWithCondition :: Phantoms.TTerm Expressions.SelectionExpression -> Phantoms.TTerm Expressions.SelectionCondition -> Phantoms.TTerm Expressions.SelectionExpression
selectionExpressionWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
              Core.projectionField = (Core.Name "expression")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

selectionExpressionWithExpression :: Phantoms.TTerm Expressions.SelectionExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.SelectionExpression
selectionExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
              Core.projectionField = (Core.Name "condition")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

simpleConditionLabelEquals :: Phantoms.TTerm Expressions.LabelCondition -> Phantoms.TTerm Expressions.SimpleCondition
simpleConditionLabelEquals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labelEquals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleConditionLengthEquals :: Phantoms.TTerm Expressions.LengthCondition -> Phantoms.TTerm Expressions.SimpleCondition
simpleConditionLengthEquals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lengthEquals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleConditionPropertyComparison :: Phantoms.TTerm Expressions.PropertyComparisonCondition -> Phantoms.TTerm Expressions.SimpleCondition
simpleConditionPropertyComparison x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyComparison"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleConditionPropertyEquals :: Phantoms.TTerm Expressions.PropertyCondition -> Phantoms.TTerm Expressions.SimpleCondition
simpleConditionPropertyEquals x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyEquals"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

solutionSpaceExpressionGroupBy :: Phantoms.TTerm Expressions.GroupByExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression
solutionSpaceExpressionGroupBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

solutionSpaceExpressionOrderBy :: Phantoms.TTerm Expressions.OrderByExpression -> Phantoms.TTerm Expressions.SolutionSpaceExpression
solutionSpaceExpressionOrderBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orderBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

unGraphReference :: Phantoms.TTerm Expressions.GraphReference -> Phantoms.TTerm String
unGraphReference x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "com.gdblab.pathAlgebra.expressions.GraphReference")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionExpression :: Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.UnionExpression
unionExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

unionExpressionLeft :: Phantoms.TTerm Expressions.UnionExpression -> Phantoms.TTerm Expressions.PathExpression
unionExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionExpressionRight :: Phantoms.TTerm Expressions.UnionExpression -> Phantoms.TTerm Expressions.PathExpression
unionExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionExpressionWithLeft :: Phantoms.TTerm Expressions.UnionExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.UnionExpression
unionExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionExpressionWithRight :: Phantoms.TTerm Expressions.UnionExpression -> Phantoms.TTerm Expressions.PathExpression -> Phantoms.TTerm Expressions.UnionExpression
unionExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
