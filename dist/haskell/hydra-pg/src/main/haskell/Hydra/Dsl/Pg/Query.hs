-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.query

module Hydra.Dsl.Pg.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.Query as Query
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

aggregationQueryCount :: Phantoms.TypedTerm Query.AggregationQuery
aggregationQueryCount =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.AggregationQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = Core.TermUnit}}))

applicationQuery :: Phantoms.TypedTerm [Query.Query] -> Phantoms.TypedTerm Query.ApplicationQuery
applicationQuery x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.ApplicationQuery"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

associativeExpression :: Phantoms.TypedTerm Query.BinaryOperator -> Phantoms.TypedTerm [Query.Expression] -> Phantoms.TypedTerm Query.AssociativeExpression
associativeExpression operator operands =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Phantoms.unTypedTerm operands)}]}))

associativeExpressionOperands :: Phantoms.TypedTerm Query.AssociativeExpression -> Phantoms.TypedTerm [Query.Expression]
associativeExpressionOperands x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
        Core.projectionFieldName = (Core.Name "operands")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

associativeExpressionOperator :: Phantoms.TypedTerm Query.AssociativeExpression -> Phantoms.TypedTerm Query.BinaryOperator
associativeExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

associativeExpressionWithOperands :: Phantoms.TypedTerm Query.AssociativeExpression -> Phantoms.TypedTerm [Query.Expression] -> Phantoms.TypedTerm Query.AssociativeExpression
associativeExpressionWithOperands original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

associativeExpressionWithOperator :: Phantoms.TypedTerm Query.AssociativeExpression -> Phantoms.TypedTerm Query.BinaryOperator -> Phantoms.TypedTerm Query.AssociativeExpression
associativeExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
              Core.projectionFieldName = (Core.Name "operands")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

binaryBooleanOperatorAnd :: Phantoms.TypedTerm Query.BinaryBooleanOperator
binaryBooleanOperatorAnd =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

binaryBooleanOperatorOr :: Phantoms.TypedTerm Query.BinaryBooleanOperator
binaryBooleanOperatorOr =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

binaryBooleanOperatorXor :: Phantoms.TypedTerm Query.BinaryBooleanOperator
binaryBooleanOperatorXor =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))

binaryExpression :: Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.BinaryOperator -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.BinaryExpression
binaryExpression left operator right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

binaryExpressionLeft :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.Expression
binaryExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

binaryExpressionOperator :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.BinaryOperator
binaryExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

binaryExpressionRight :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.Expression
binaryExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

binaryExpressionWithLeft :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.BinaryExpression
binaryExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

binaryExpressionWithOperator :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.BinaryOperator -> Phantoms.TypedTerm Query.BinaryExpression
binaryExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

binaryExpressionWithRight :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.BinaryExpression
binaryExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

binaryOperatorBoolean :: Phantoms.TypedTerm Query.BinaryBooleanOperator -> Phantoms.TypedTerm Query.BinaryOperator
binaryOperatorBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

binaryOperatorComparison :: Phantoms.TypedTerm Query.ComparisonOperator -> Phantoms.TypedTerm Query.BinaryOperator
binaryOperatorComparison x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "comparison"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

binaryOperatorPower :: Phantoms.TypedTerm Query.BinaryOperator
binaryOperatorPower =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "power"),
        Core.fieldTerm = Core.TermUnit}}))

binding :: Phantoms.TypedTerm Query.Variable -> Phantoms.TypedTerm Query.Query -> Phantoms.TypedTerm Query.Binding
binding key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

bindingKey :: Phantoms.TypedTerm Query.Binding -> Phantoms.TypedTerm Query.Variable
bindingKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

bindingValue :: Phantoms.TypedTerm Query.Binding -> Phantoms.TypedTerm Query.Query
bindingValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

bindingWithKey :: Phantoms.TypedTerm Query.Binding -> Phantoms.TypedTerm Query.Variable -> Phantoms.TypedTerm Query.Binding
bindingWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

bindingWithValue :: Phantoms.TypedTerm Query.Binding -> Phantoms.TypedTerm Query.Query -> Phantoms.TypedTerm Query.Binding
bindingWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

comparisonOperatorEq :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorEq =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGt :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorGt =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGte :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorGte =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLt :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorLt =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLte :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorLte =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNeq :: Phantoms.TypedTerm Query.ComparisonOperator
comparisonOperatorNeq =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = Core.TermUnit}}))

edgeProjectionPattern :: Phantoms.TypedTerm Model.Direction -> Phantoms.TypedTerm (Maybe Model.EdgeLabel) -> Phantoms.TypedTerm [Query.PropertyPattern] -> Phantoms.TypedTerm (Maybe Query.VertexPattern) -> Phantoms.TypedTerm Query.EdgeProjectionPattern
edgeProjectionPattern direction label properties vertex =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm vertex)}]}))

edgeProjectionPatternDirection :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm Model.Direction
edgeProjectionPatternDirection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionFieldName = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeProjectionPatternLabel :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm (Maybe Model.EdgeLabel)
edgeProjectionPatternLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeProjectionPatternProperties :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm [Query.PropertyPattern]
edgeProjectionPatternProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeProjectionPatternVertex :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm (Maybe Query.VertexPattern)
edgeProjectionPatternVertex x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionFieldName = (Core.Name "vertex")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeProjectionPatternWithDirection :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm Model.Direction -> Phantoms.TypedTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithDirection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeProjectionPatternWithLabel :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm (Maybe Model.EdgeLabel) -> Phantoms.TypedTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeProjectionPatternWithProperties :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm [Query.PropertyPattern] -> Phantoms.TypedTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "vertex")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeProjectionPatternWithVertex :: Phantoms.TypedTerm Query.EdgeProjectionPattern -> Phantoms.TypedTerm (Maybe Query.VertexPattern) -> Phantoms.TypedTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithVertex original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

expressionAssociative :: Phantoms.TypedTerm Query.AssociativeExpression -> Phantoms.TypedTerm Query.Expression
expressionAssociative x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "associative"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

expressionBinary :: Phantoms.TypedTerm Query.BinaryExpression -> Phantoms.TypedTerm Query.Expression
expressionBinary x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

expressionProperty :: Phantoms.TypedTerm Query.PropertyProjection -> Phantoms.TypedTerm Query.Expression
expressionProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

expressionUnary :: Phantoms.TypedTerm Query.UnaryExpression -> Phantoms.TypedTerm Query.Expression
expressionUnary x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

expressionVariable :: Phantoms.TypedTerm Query.Variable -> Phantoms.TypedTerm Query.Expression
expressionVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

expressionVertex :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm Query.Expression
expressionVertex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

letQuery :: Phantoms.TypedTerm [Query.Binding] -> Phantoms.TypedTerm Query.Query -> Phantoms.TypedTerm Query.LetQuery
letQuery bindings environment =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTypedTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Phantoms.unTypedTerm environment)}]}))

letQueryBindings :: Phantoms.TypedTerm Query.LetQuery -> Phantoms.TypedTerm [Query.Binding]
letQueryBindings x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
        Core.projectionFieldName = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

letQueryEnvironment :: Phantoms.TypedTerm Query.LetQuery -> Phantoms.TypedTerm Query.Query
letQueryEnvironment x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
        Core.projectionFieldName = (Core.Name "environment")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

letQueryWithBindings :: Phantoms.TypedTerm Query.LetQuery -> Phantoms.TypedTerm [Query.Binding] -> Phantoms.TypedTerm Query.LetQuery
letQueryWithBindings original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
              Core.projectionFieldName = (Core.Name "environment")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

letQueryWithEnvironment :: Phantoms.TypedTerm Query.LetQuery -> Phantoms.TypedTerm Query.Query -> Phantoms.TypedTerm Query.LetQuery
letQueryWithEnvironment original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
              Core.projectionFieldName = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

matchQuery :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [Query.Projection] -> Phantoms.TypedTerm (Maybe Query.Expression) -> Phantoms.TypedTerm Query.MatchQuery
matchQuery optional pattern where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTypedTerm optional)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)}]}))

matchQueryOptional :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm Bool
matchQueryOptional x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionFieldName = (Core.Name "optional")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchQueryPattern :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm [Query.Projection]
matchQueryPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchQueryWhere :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm (Maybe Query.Expression)
matchQueryWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchQueryWithOptional :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Query.MatchQuery
matchQueryWithOptional original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

matchQueryWithPattern :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm [Query.Projection] -> Phantoms.TypedTerm Query.MatchQuery
matchQueryWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

matchQueryWithWhere :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm (Maybe Query.Expression) -> Phantoms.TypedTerm Query.MatchQuery
matchQueryWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projection :: Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm (Maybe Query.Variable) -> Phantoms.TypedTerm Query.Projection
projection value as =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm as)}]}))

projectionAs :: Phantoms.TypedTerm Query.Projection -> Phantoms.TypedTerm (Maybe Query.Variable)
projectionAs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
        Core.projectionFieldName = (Core.Name "as")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionValue :: Phantoms.TypedTerm Query.Projection -> Phantoms.TypedTerm Query.Expression
projectionValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionWithAs :: Phantoms.TypedTerm Query.Projection -> Phantoms.TypedTerm (Maybe Query.Variable) -> Phantoms.TypedTerm Query.Projection
projectionWithAs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projectionWithValue :: Phantoms.TypedTerm Query.Projection -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.Projection
projectionWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
              Core.projectionFieldName = (Core.Name "as")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projections :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [Query.Projection] -> Phantoms.TypedTerm Query.Projections
projections all explicit =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTypedTerm all)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTypedTerm explicit)}]}))

projectionsAll :: Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm Bool
projectionsAll x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
        Core.projectionFieldName = (Core.Name "all")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionsExplicit :: Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm [Query.Projection]
projectionsExplicit x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
        Core.projectionFieldName = (Core.Name "explicit")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionsWithAll :: Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Query.Projections
projectionsWithAll original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
              Core.projectionFieldName = (Core.Name "explicit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionsWithExplicit :: Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm [Query.Projection] -> Phantoms.TypedTerm Query.Projections
projectionsWithExplicit original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
              Core.projectionFieldName = (Core.Name "all")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyPattern :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Query.PropertyValuePattern -> Phantoms.TypedTerm Query.PropertyPattern
propertyPattern key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

propertyPatternKey :: Phantoms.TypedTerm Query.PropertyPattern -> Phantoms.TypedTerm Model.PropertyKey
propertyPatternKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyPatternValue :: Phantoms.TypedTerm Query.PropertyPattern -> Phantoms.TypedTerm Query.PropertyValuePattern
propertyPatternValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyPatternWithKey :: Phantoms.TypedTerm Query.PropertyPattern -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Query.PropertyPattern
propertyPatternWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyPatternWithValue :: Phantoms.TypedTerm Query.PropertyPattern -> Phantoms.TypedTerm Query.PropertyValuePattern -> Phantoms.TypedTerm Query.PropertyPattern
propertyPatternWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyProjection :: Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Query.PropertyProjection
propertyProjection base key =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTypedTerm base)},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)}]}))

propertyProjectionBase :: Phantoms.TypedTerm Query.PropertyProjection -> Phantoms.TypedTerm Query.Expression
propertyProjectionBase x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
        Core.projectionFieldName = (Core.Name "base")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyProjectionKey :: Phantoms.TypedTerm Query.PropertyProjection -> Phantoms.TypedTerm Model.PropertyKey
propertyProjectionKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyProjectionWithBase :: Phantoms.TypedTerm Query.PropertyProjection -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.PropertyProjection
propertyProjectionWithBase original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyProjectionWithKey :: Phantoms.TypedTerm Query.PropertyProjection -> Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Query.PropertyProjection
propertyProjectionWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
              Core.projectionFieldName = (Core.Name "base")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyValue :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Query.PropertyValue
propertyValue x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.PropertyValue"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

propertyValuePatternValue :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Query.PropertyValuePattern
propertyValuePatternValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.PropertyValuePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyValuePatternVariable :: Phantoms.TypedTerm Model.PropertyKey -> Phantoms.TypedTerm Query.PropertyValuePattern
propertyValuePatternVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.PropertyValuePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryAggregate :: Phantoms.TypedTerm Query.AggregationQuery -> Phantoms.TypedTerm Query.Query
queryAggregate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aggregate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryApplication :: Phantoms.TypedTerm Query.ApplicationQuery -> Phantoms.TypedTerm Query.Query
queryApplication x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryLetQuery :: Phantoms.TypedTerm Query.LetQuery -> Phantoms.TypedTerm Query.Query
queryLetQuery x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "LetQuery"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryMatch :: Phantoms.TypedTerm Query.MatchQuery -> Phantoms.TypedTerm Query.Query
queryMatch x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

querySelect :: Phantoms.TypedTerm Query.SelectQuery -> Phantoms.TypedTerm Query.Query
querySelect x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryValue :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Query.Query
queryValue x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

selectQuery :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm Query.SelectQuery
selectQuery distinct projection =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm projection)}]}))

selectQueryDistinct :: Phantoms.TypedTerm Query.SelectQuery -> Phantoms.TypedTerm Bool
selectQueryDistinct x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
        Core.projectionFieldName = (Core.Name "distinct")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

selectQueryProjection :: Phantoms.TypedTerm Query.SelectQuery -> Phantoms.TypedTerm Query.Projections
selectQueryProjection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
        Core.projectionFieldName = (Core.Name "projection")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

selectQueryWithDistinct :: Phantoms.TypedTerm Query.SelectQuery -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Query.SelectQuery
selectQueryWithDistinct original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

selectQueryWithProjection :: Phantoms.TypedTerm Query.SelectQuery -> Phantoms.TypedTerm Query.Projections -> Phantoms.TypedTerm Query.SelectQuery
selectQueryWithProjection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unApplicationQuery :: Phantoms.TypedTerm Query.ApplicationQuery -> Phantoms.TypedTerm [Query.Query]
unApplicationQuery x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.query.ApplicationQuery")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPropertyValue :: Phantoms.TypedTerm Query.PropertyValue -> Phantoms.TypedTerm String
unPropertyValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.query.PropertyValue")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unVariable :: Phantoms.TypedTerm Query.Variable -> Phantoms.TypedTerm String
unVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.pg.query.Variable")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryExpression :: Phantoms.TypedTerm Query.UnaryOperator -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.UnaryExpression
unaryExpression operator operand =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTypedTerm operand)}]}))

unaryExpressionOperand :: Phantoms.TypedTerm Query.UnaryExpression -> Phantoms.TypedTerm Query.Expression
unaryExpressionOperand x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
        Core.projectionFieldName = (Core.Name "operand")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryExpressionOperator :: Phantoms.TypedTerm Query.UnaryExpression -> Phantoms.TypedTerm Query.UnaryOperator
unaryExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryExpressionWithOperand :: Phantoms.TypedTerm Query.UnaryExpression -> Phantoms.TypedTerm Query.Expression -> Phantoms.TypedTerm Query.UnaryExpression
unaryExpressionWithOperand original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unaryExpressionWithOperator :: Phantoms.TypedTerm Query.UnaryExpression -> Phantoms.TypedTerm Query.UnaryOperator -> Phantoms.TypedTerm Query.UnaryExpression
unaryExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
              Core.projectionFieldName = (Core.Name "operand")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

unaryOperatorNegate :: Phantoms.TypedTerm Query.UnaryOperator
unaryOperatorNegate =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = Core.TermUnit}}))

variable :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Query.Variable
variable x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.Variable"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

vertexPattern :: Phantoms.TypedTerm (Maybe Query.Variable) -> Phantoms.TypedTerm (Maybe Model.VertexLabel) -> Phantoms.TypedTerm [Query.PropertyPattern] -> Phantoms.TypedTerm [Query.EdgeProjectionPattern] -> Phantoms.TypedTerm Query.VertexPattern
vertexPattern variable label properties edges =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm edges)}]}))

vertexPatternEdges :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm [Query.EdgeProjectionPattern]
vertexPatternEdges x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionFieldName = (Core.Name "edges")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPatternLabel :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm (Maybe Model.VertexLabel)
vertexPatternLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionFieldName = (Core.Name "label")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPatternProperties :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm [Query.PropertyPattern]
vertexPatternProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPatternVariable :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm (Maybe Query.Variable)
vertexPatternVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

vertexPatternWithEdges :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm [Query.EdgeProjectionPattern] -> Phantoms.TypedTerm Query.VertexPattern
vertexPatternWithEdges original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

vertexPatternWithLabel :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm (Maybe Model.VertexLabel) -> Phantoms.TypedTerm Query.VertexPattern
vertexPatternWithLabel original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPatternWithProperties :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm [Query.PropertyPattern] -> Phantoms.TypedTerm Query.VertexPattern
vertexPatternWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

vertexPatternWithVariable :: Phantoms.TypedTerm Query.VertexPattern -> Phantoms.TypedTerm (Maybe Query.Variable) -> Phantoms.TypedTerm Query.VertexPattern
vertexPatternWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "label")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionFieldName = (Core.Name "edges")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))
