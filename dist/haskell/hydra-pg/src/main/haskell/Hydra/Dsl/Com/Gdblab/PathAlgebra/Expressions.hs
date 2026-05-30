-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for com.gdblab.pathAlgebra.expressions

module Hydra.Dsl.Com.Gdblab.PathAlgebra.Expressions where

import qualified Com.Gdblab.PathAlgebra.Expressions as Expressions
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

andCondition :: Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.AndCondition
andCondition left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

andConditionLeft :: Phantoms.TypedTerm Expressions.AndCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
andConditionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

andConditionRight :: Phantoms.TypedTerm Expressions.AndCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
andConditionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

andConditionWithLeft :: Phantoms.TypedTerm Expressions.AndCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.AndCondition
andConditionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

andConditionWithRight :: Phantoms.TypedTerm Expressions.AndCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.AndCondition
andConditionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.AndCondition"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

baseExpressionPaths0 :: Phantoms.TypedTerm Expressions.GraphReference -> Phantoms.TypedTerm Expressions.BaseExpression
baseExpressionPaths0 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paths0"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

baseExpressionPaths1 :: Phantoms.TypedTerm Expressions.GraphReference -> Phantoms.TypedTerm Expressions.BaseExpression
baseExpressionPaths1 x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "paths1"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

baseExpressionPathsStar :: Phantoms.TypedTerm Expressions.GraphReference -> Phantoms.TypedTerm Expressions.BaseExpression
baseExpressionPathsStar x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.BaseExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pathsStar"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

comparisonOperatorEqual :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGreaterThan :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorGreaterThan =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGreaterThanOrEqual :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorGreaterThanOrEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLessThan :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorLessThan =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLessThanOrEqual :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorLessThanOrEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNotEqual :: Phantoms.TypedTerm Expressions.ComparisonOperator
comparisonOperatorNotEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

edgePropertyRef :: Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.EdgePropertyRef
edgePropertyRef element property =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTypedTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm property)}]}))

edgePropertyRefElement :: Phantoms.TypedTerm Expressions.EdgePropertyRef -> Phantoms.TypedTerm Expressions.PathElement
edgePropertyRefElement x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgePropertyRefProperty :: Phantoms.TypedTerm Expressions.EdgePropertyRef -> Phantoms.TypedTerm String
edgePropertyRefProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
        Core.projectionFieldName = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgePropertyRefWithElement :: Phantoms.TypedTerm Expressions.EdgePropertyRef -> Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm Expressions.EdgePropertyRef
edgePropertyRefWithElement original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgePropertyRefWithProperty :: Phantoms.TypedTerm Expressions.EdgePropertyRef -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.EdgePropertyRef
edgePropertyRefWithProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.EdgePropertyRef"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

graphReference :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.GraphReference
graphReference x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GraphReference"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

groupByCriterionLength :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionNone :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionNone =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSource :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionSource =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceLength :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionSourceLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceTarget :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionSourceTarget =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTarget"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionSourceTargetLength :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionSourceTargetLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTargetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionTarget :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionTarget =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

groupByCriterionTargetLength :: Phantoms.TypedTerm Expressions.GroupByCriterion
groupByCriterionTargetLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByExpression :: Phantoms.TypedTerm Expressions.GroupByCriterion -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.GroupByExpression
groupByExpression criterion expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTypedTerm criterion)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

groupByExpressionCriterion :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.GroupByCriterion
groupByExpressionCriterion x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
        Core.projectionFieldName = (Core.Name "criterion")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

groupByExpressionExpression :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.PathExpression
groupByExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

groupByExpressionWithCriterion :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.GroupByCriterion -> Phantoms.TypedTerm Expressions.GroupByExpression
groupByExpressionWithCriterion original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

groupByExpressionWithExpression :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.GroupByExpression
groupByExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.GroupByExpression"),
              Core.projectionFieldName = (Core.Name "criterion")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

joinExpression :: Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.JoinExpression
joinExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

joinExpressionLeft :: Phantoms.TypedTerm Expressions.JoinExpression -> Phantoms.TypedTerm Expressions.PathExpression
joinExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

joinExpressionRight :: Phantoms.TypedTerm Expressions.JoinExpression -> Phantoms.TypedTerm Expressions.PathExpression
joinExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

joinExpressionWithLeft :: Phantoms.TypedTerm Expressions.JoinExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.JoinExpression
joinExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

joinExpressionWithRight :: Phantoms.TypedTerm Expressions.JoinExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.JoinExpression
joinExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.JoinExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

labelCondition :: Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.LabelCondition
labelCondition target value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

labelConditionTarget :: Phantoms.TypedTerm Expressions.LabelCondition -> Phantoms.TypedTerm Expressions.PathElement
labelConditionTarget x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

labelConditionValue :: Phantoms.TypedTerm Expressions.LabelCondition -> Phantoms.TypedTerm String
labelConditionValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

labelConditionWithTarget :: Phantoms.TypedTerm Expressions.LabelCondition -> Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm Expressions.LabelCondition
labelConditionWithTarget original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

labelConditionWithValue :: Phantoms.TypedTerm Expressions.LabelCondition -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.LabelCondition
labelConditionWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LabelCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

lengthCondition :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.LengthCondition
lengthCondition length =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTypedTerm length)}]}))

lengthConditionLength :: Phantoms.TypedTerm Expressions.LengthCondition -> Phantoms.TypedTerm Int
lengthConditionLength x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
        Core.projectionFieldName = (Core.Name "length")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

lengthConditionWithLength :: Phantoms.TypedTerm Expressions.LengthCondition -> Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.LengthCondition
lengthConditionWithLength original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LengthCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "length"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

literalValueBoolean :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Expressions.LiteralValue
literalValueBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalValueFloat :: Phantoms.TypedTerm Double -> Phantoms.TypedTerm Expressions.LiteralValue
literalValueFloat x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalValueInteger :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.LiteralValue
literalValueInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalValueString :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.LiteralValue
literalValueString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.LiteralValue"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nodePropertyRef :: Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.NodePropertyRef
nodePropertyRef element property =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTypedTerm element)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm property)}]}))

nodePropertyRefElement :: Phantoms.TypedTerm Expressions.NodePropertyRef -> Phantoms.TypedTerm Expressions.PathElement
nodePropertyRefElement x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
        Core.projectionFieldName = (Core.Name "element")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePropertyRefProperty :: Phantoms.TypedTerm Expressions.NodePropertyRef -> Phantoms.TypedTerm String
nodePropertyRefProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
        Core.projectionFieldName = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePropertyRefWithElement :: Phantoms.TypedTerm Expressions.NodePropertyRef -> Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm Expressions.NodePropertyRef
nodePropertyRefWithElement original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodePropertyRefWithProperty :: Phantoms.TypedTerm Expressions.NodePropertyRef -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.NodePropertyRef
nodePropertyRefWithProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "element"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NodePropertyRef"),
              Core.projectionFieldName = (Core.Name "element")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

notCondition :: Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.NotCondition
notCondition condition =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm condition)}]}))

notConditionCondition :: Phantoms.TypedTerm Expressions.NotCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
notConditionCondition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

notConditionWithCondition :: Phantoms.TypedTerm Expressions.NotCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.NotCondition
notConditionWithCondition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.NotCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

orCondition :: Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.OrCondition
orCondition left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

orConditionLeft :: Phantoms.TypedTerm Expressions.OrCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
orConditionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

orConditionRight :: Phantoms.TypedTerm Expressions.OrCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
orConditionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

orConditionWithLeft :: Phantoms.TypedTerm Expressions.OrCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.OrCondition
orConditionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

orConditionWithRight :: Phantoms.TypedTerm Expressions.OrCondition -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.OrCondition
orConditionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrCondition"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

orderByCriterionGroup :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionGroup =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionGroupPath :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionGroupPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartition :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionPartition =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partition"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionGroup :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionPartitionGroup =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroup"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionGroupPath :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionPartitionGroupPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPartitionPath :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionPartitionPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByCriterionPath :: Phantoms.TypedTerm Expressions.OrderByCriterion
orderByCriterionPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByCriterion"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

orderByExpression :: Phantoms.TypedTerm Expressions.OrderByCriterion -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression -> Phantoms.TypedTerm Expressions.OrderByExpression
orderByExpression criterion expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTypedTerm criterion)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

orderByExpressionCriterion :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.OrderByCriterion
orderByExpressionCriterion x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
        Core.projectionFieldName = (Core.Name "criterion")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

orderByExpressionExpression :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression
orderByExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

orderByExpressionWithCriterion :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.OrderByCriterion -> Phantoms.TypedTerm Expressions.OrderByExpression
orderByExpressionWithCriterion original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

orderByExpressionWithExpression :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression -> Phantoms.TypedTerm Expressions.OrderByExpression
orderByExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "criterion"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.OrderByExpression"),
              Core.projectionFieldName = (Core.Name "criterion")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pathElementEdge :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.PathElement
pathElementEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathElementFirst :: Phantoms.TypedTerm Expressions.PathElement
pathElementFirst =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "first"),
        Core.fieldTerm = Core.TermUnit}}))

pathElementLast :: Phantoms.TypedTerm Expressions.PathElement
pathElementLast =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "last"),
        Core.fieldTerm = Core.TermUnit}}))

pathElementNode :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.PathElement
pathElementNode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionBase :: Phantoms.TypedTerm Expressions.BaseExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionBase x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "base"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionGroupBy :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionGroupBy x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupBy"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionJoin :: Phantoms.TypedTerm Expressions.JoinExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionJoin x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "join"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionOrderBy :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionOrderBy x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orderBy"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionProjection :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionProjection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionRecursive :: Phantoms.TypedTerm Expressions.RecursiveExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionRecursive x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "recursive"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionSelection :: Phantoms.TypedTerm Expressions.SelectionExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionSelection x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "selection"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathExpressionUnion :: Phantoms.TypedTerm Expressions.UnionExpression -> Phantoms.TypedTerm Expressions.PathExpression
pathExpressionUnion x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathPropertyRefEndNode :: Phantoms.TypedTerm Expressions.PathPropertyRef
pathPropertyRefEndNode =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endNode"),
        Core.fieldTerm = Core.TermUnit}}))

pathPropertyRefLength :: Phantoms.TypedTerm Expressions.PathPropertyRef
pathPropertyRefLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

pathPropertyRefStartNode :: Phantoms.TypedTerm Expressions.PathPropertyRef
pathPropertyRefStartNode =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathPropertyRef"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startNode"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsAcyclic :: Phantoms.TypedTerm Expressions.PathSemantics
pathSemanticsAcyclic =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "acyclic"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsShortest :: Phantoms.TypedTerm Expressions.PathSemantics
pathSemanticsShortest =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortest"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsSimple :: Phantoms.TypedTerm Expressions.PathSemantics
pathSemanticsSimple =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsTrail :: Phantoms.TypedTerm Expressions.PathSemantics
pathSemanticsTrail =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trail"),
        Core.fieldTerm = Core.TermUnit}}))

pathSemanticsWalk :: Phantoms.TypedTerm Expressions.PathSemantics
pathSemanticsWalk =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PathSemantics"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "walk"),
        Core.fieldTerm = Core.TermUnit}}))

projectionExpression :: Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression -> Phantoms.TypedTerm Expressions.ProjectionExpression
projectionExpression partitions groups paths expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Phantoms.unTypedTerm partitions)},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTypedTerm groups)},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Phantoms.unTypedTerm paths)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

projectionExpressionExpression :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression
projectionExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionExpressionGroups :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec
projectionExpressionGroups x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionFieldName = (Core.Name "groups")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionExpressionPartitions :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec
projectionExpressionPartitions x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionFieldName = (Core.Name "partitions")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionExpressionPaths :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec
projectionExpressionPaths x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
        Core.projectionFieldName = (Core.Name "paths")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionExpressionWithExpression :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression -> Phantoms.TypedTerm Expressions.ProjectionExpression
projectionExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "partitions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "groups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "paths")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projectionExpressionWithGroups :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.ProjectionExpression
projectionExpressionWithGroups original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "partitions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "paths")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionExpressionWithPartitions :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.ProjectionExpression
projectionExpressionWithPartitions original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "groups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "paths")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionExpressionWithPaths :: Phantoms.TypedTerm Expressions.ProjectionExpression -> Phantoms.TypedTerm Expressions.ProjectionSpec -> Phantoms.TypedTerm Expressions.ProjectionExpression
projectionExpressionWithPaths original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "partitions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "groups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "paths"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionSpecAll :: Phantoms.TypedTerm Expressions.ProjectionSpec
projectionSpecAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

projectionSpecLimited :: Phantoms.TypedTerm Int -> Phantoms.TypedTerm Expressions.ProjectionSpec
projectionSpecLimited x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ProjectionSpec"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyComparisonCondition :: Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.ComparisonOperator -> Phantoms.TypedTerm Expressions.LiteralValue -> Phantoms.TypedTerm Expressions.PropertyComparisonCondition
propertyComparisonCondition target property operator value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

propertyComparisonConditionOperator :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.ComparisonOperator
propertyComparisonConditionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyComparisonConditionProperty :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm String
propertyComparisonConditionProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionFieldName = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyComparisonConditionTarget :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.PathElement
propertyComparisonConditionTarget x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyComparisonConditionValue :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.LiteralValue
propertyComparisonConditionValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyComparisonConditionWithOperator :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.ComparisonOperator -> Phantoms.TypedTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyComparisonConditionWithProperty :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyComparisonConditionWithTarget :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithTarget original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyComparisonConditionWithValue :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.LiteralValue -> Phantoms.TypedTerm Expressions.PropertyComparisonCondition
propertyComparisonConditionWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyCondition :: Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.LiteralValue -> Phantoms.TypedTerm Expressions.PropertyCondition
propertyCondition target property value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm property)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

propertyConditionProperty :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm String
propertyConditionProperty x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionFieldName = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyConditionTarget :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm Expressions.PathElement
propertyConditionTarget x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyConditionValue :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm Expressions.LiteralValue
propertyConditionValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyConditionWithProperty :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm String -> Phantoms.TypedTerm Expressions.PropertyCondition
propertyConditionWithProperty original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyConditionWithTarget :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm Expressions.PathElement -> Phantoms.TypedTerm Expressions.PropertyCondition
propertyConditionWithTarget original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyConditionWithValue :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm Expressions.LiteralValue -> Phantoms.TypedTerm Expressions.PropertyCondition
propertyConditionWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyCondition"),
              Core.projectionFieldName = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyExtraction :: Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Expressions.PropertySource -> Phantoms.TypedTerm Expressions.PropertyExtraction
propertyExtraction alias source =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTypedTerm alias)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm source)}]}))

propertyExtractionAlias :: Phantoms.TypedTerm Expressions.PropertyExtraction -> Phantoms.TypedTerm (Maybe String)
propertyExtractionAlias x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
        Core.projectionFieldName = (Core.Name "alias")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyExtractionSource :: Phantoms.TypedTerm Expressions.PropertyExtraction -> Phantoms.TypedTerm Expressions.PropertySource
propertyExtractionSource x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyExtractionWithAlias :: Phantoms.TypedTerm Expressions.PropertyExtraction -> Phantoms.TypedTerm (Maybe String) -> Phantoms.TypedTerm Expressions.PropertyExtraction
propertyExtractionWithAlias original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyExtractionWithSource :: Phantoms.TypedTerm Expressions.PropertyExtraction -> Phantoms.TypedTerm Expressions.PropertySource -> Phantoms.TypedTerm Expressions.PropertyExtraction
propertyExtractionWithSource original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertyExtraction"),
              Core.projectionFieldName = (Core.Name "alias")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertySourceEdgeProperty :: Phantoms.TypedTerm Expressions.EdgePropertyRef -> Phantoms.TypedTerm Expressions.PropertySource
propertySourceEdgeProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edgeProperty"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertySourceNodeProperty :: Phantoms.TypedTerm Expressions.NodePropertyRef -> Phantoms.TypedTerm Expressions.PropertySource
propertySourceNodeProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nodeProperty"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertySourcePathProperty :: Phantoms.TypedTerm Expressions.PathPropertyRef -> Phantoms.TypedTerm Expressions.PropertySource
propertySourcePathProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.PropertySource"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pathProperty"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryExpression :: Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm (Maybe Expressions.ResultProjection) -> Phantoms.TypedTerm Expressions.QueryExpression
queryExpression pathExpression resultProjection =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Phantoms.unTypedTerm pathExpression)},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Phantoms.unTypedTerm resultProjection)}]}))

queryExpressionPathExpression :: Phantoms.TypedTerm Expressions.QueryExpression -> Phantoms.TypedTerm Expressions.PathExpression
queryExpressionPathExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
        Core.projectionFieldName = (Core.Name "pathExpression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

queryExpressionResultProjection :: Phantoms.TypedTerm Expressions.QueryExpression -> Phantoms.TypedTerm (Maybe Expressions.ResultProjection)
queryExpressionResultProjection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
        Core.projectionFieldName = (Core.Name "resultProjection")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

queryExpressionWithPathExpression :: Phantoms.TypedTerm Expressions.QueryExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.QueryExpression
queryExpressionWithPathExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
              Core.projectionFieldName = (Core.Name "resultProjection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

queryExpressionWithResultProjection :: Phantoms.TypedTerm Expressions.QueryExpression -> Phantoms.TypedTerm (Maybe Expressions.ResultProjection) -> Phantoms.TypedTerm Expressions.QueryExpression
queryExpressionWithResultProjection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathExpression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.QueryExpression"),
              Core.projectionFieldName = (Core.Name "pathExpression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "resultProjection"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

recursiveExpression :: Phantoms.TypedTerm Expressions.PathSemantics -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.RecursiveExpression
recursiveExpression semantics expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Phantoms.unTypedTerm semantics)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

recursiveExpressionExpression :: Phantoms.TypedTerm Expressions.RecursiveExpression -> Phantoms.TypedTerm Expressions.PathExpression
recursiveExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

recursiveExpressionSemantics :: Phantoms.TypedTerm Expressions.RecursiveExpression -> Phantoms.TypedTerm Expressions.PathSemantics
recursiveExpressionSemantics x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
        Core.projectionFieldName = (Core.Name "semantics")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

recursiveExpressionWithExpression :: Phantoms.TypedTerm Expressions.RecursiveExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.RecursiveExpression
recursiveExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
              Core.projectionFieldName = (Core.Name "semantics")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

recursiveExpressionWithSemantics :: Phantoms.TypedTerm Expressions.RecursiveExpression -> Phantoms.TypedTerm Expressions.PathSemantics -> Phantoms.TypedTerm Expressions.RecursiveExpression
recursiveExpressionWithSemantics original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "semantics"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.RecursiveExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

resultProjection :: Phantoms.TypedTerm [Expressions.PropertyExtraction] -> Phantoms.TypedTerm Expressions.ResultProjection
resultProjection projections =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projections"),
          Core.fieldTerm = (Phantoms.unTypedTerm projections)}]}))

resultProjectionProjections :: Phantoms.TypedTerm Expressions.ResultProjection -> Phantoms.TypedTerm [Expressions.PropertyExtraction]
resultProjectionProjections x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
        Core.projectionFieldName = (Core.Name "projections")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

resultProjectionWithProjections :: Phantoms.TypedTerm Expressions.ResultProjection -> Phantoms.TypedTerm [Expressions.PropertyExtraction] -> Phantoms.TypedTerm Expressions.ResultProjection
resultProjectionWithProjections original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.ResultProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projections"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

selectionConditionAnd :: Phantoms.TypedTerm Expressions.AndCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
selectionConditionAnd x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectionConditionNot :: Phantoms.TypedTerm Expressions.NotCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
selectionConditionNot x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectionConditionOr :: Phantoms.TypedTerm Expressions.OrCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
selectionConditionOr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectionConditionSimple :: Phantoms.TypedTerm Expressions.SimpleCondition -> Phantoms.TypedTerm Expressions.SelectionCondition
selectionConditionSimple x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectionExpression :: Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.SelectionExpression
selectionExpression condition expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

selectionExpressionCondition :: Phantoms.TypedTerm Expressions.SelectionExpression -> Phantoms.TypedTerm Expressions.SelectionCondition
selectionExpressionCondition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

selectionExpressionExpression :: Phantoms.TypedTerm Expressions.SelectionExpression -> Phantoms.TypedTerm Expressions.PathExpression
selectionExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

selectionExpressionWithCondition :: Phantoms.TypedTerm Expressions.SelectionExpression -> Phantoms.TypedTerm Expressions.SelectionCondition -> Phantoms.TypedTerm Expressions.SelectionExpression
selectionExpressionWithCondition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

selectionExpressionWithExpression :: Phantoms.TypedTerm Expressions.SelectionExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.SelectionExpression
selectionExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SelectionExpression"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

simpleConditionLabelEquals :: Phantoms.TypedTerm Expressions.LabelCondition -> Phantoms.TypedTerm Expressions.SimpleCondition
simpleConditionLabelEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "labelEquals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

simpleConditionLengthEquals :: Phantoms.TypedTerm Expressions.LengthCondition -> Phantoms.TypedTerm Expressions.SimpleCondition
simpleConditionLengthEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lengthEquals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

simpleConditionPropertyComparison :: Phantoms.TypedTerm Expressions.PropertyComparisonCondition -> Phantoms.TypedTerm Expressions.SimpleCondition
simpleConditionPropertyComparison x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyComparison"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

simpleConditionPropertyEquals :: Phantoms.TypedTerm Expressions.PropertyCondition -> Phantoms.TypedTerm Expressions.SimpleCondition
simpleConditionPropertyEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SimpleCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "propertyEquals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

solutionSpaceExpressionGroupBy :: Phantoms.TypedTerm Expressions.GroupByExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression
solutionSpaceExpressionGroupBy x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupBy"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

solutionSpaceExpressionOrderBy :: Phantoms.TypedTerm Expressions.OrderByExpression -> Phantoms.TypedTerm Expressions.SolutionSpaceExpression
solutionSpaceExpressionOrderBy x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orderBy"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

unGraphReference :: Phantoms.TypedTerm Expressions.GraphReference -> Phantoms.TypedTerm String
unGraphReference x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.expressions.GraphReference")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unionExpression :: Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.UnionExpression
unionExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

unionExpressionLeft :: Phantoms.TypedTerm Expressions.UnionExpression -> Phantoms.TypedTerm Expressions.PathExpression
unionExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unionExpressionRight :: Phantoms.TypedTerm Expressions.UnionExpression -> Phantoms.TypedTerm Expressions.PathExpression
unionExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unionExpressionWithLeft :: Phantoms.TypedTerm Expressions.UnionExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.UnionExpression
unionExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

unionExpressionWithRight :: Phantoms.TypedTerm Expressions.UnionExpression -> Phantoms.TypedTerm Expressions.PathExpression -> Phantoms.TypedTerm Expressions.UnionExpression
unionExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.expressions.UnionExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
