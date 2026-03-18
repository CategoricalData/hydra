-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.pg.query

module Hydra.Dsl.Pg.Query where

import qualified Hydra.Core as Core
import qualified Hydra.Pg.Model as Model
import qualified Hydra.Pg.Query as Query
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

aggregationQueryCount :: Phantoms.TTerm Query.AggregationQuery
aggregationQueryCount =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.AggregationQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = Core.TermUnit}}))

applicationQuery :: Phantoms.TTerm [Query.Query] -> Phantoms.TTerm Query.ApplicationQuery
applicationQuery x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.ApplicationQuery"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unApplicationQuery :: Phantoms.TTerm Query.ApplicationQuery -> Phantoms.TTerm [Query.Query]
unApplicationQuery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.query.ApplicationQuery")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

associativeExpression :: Phantoms.TTerm Query.BinaryOperator -> Phantoms.TTerm [Query.Expression] -> Phantoms.TTerm Query.AssociativeExpression
associativeExpression operator operands =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Phantoms.unTTerm operands)}]}))

associativeExpressionOperator :: Phantoms.TTerm Query.AssociativeExpression -> Phantoms.TTerm Query.BinaryOperator
associativeExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

associativeExpressionOperands :: Phantoms.TTerm Query.AssociativeExpression -> Phantoms.TTerm [Query.Expression]
associativeExpressionOperands x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
        Core.projectionField = (Core.Name "operands")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

associativeExpressionWithOperator :: Phantoms.TTerm Query.AssociativeExpression -> Phantoms.TTerm Query.BinaryOperator -> Phantoms.TTerm Query.AssociativeExpression
associativeExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
              Core.projectionField = (Core.Name "operands")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

associativeExpressionWithOperands :: Phantoms.TTerm Query.AssociativeExpression -> Phantoms.TTerm [Query.Expression] -> Phantoms.TTerm Query.AssociativeExpression
associativeExpressionWithOperands original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.AssociativeExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operands"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryExpression :: Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.BinaryOperator -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.BinaryExpression
binaryExpression left operator right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

binaryExpressionLeft :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.Expression
binaryExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionOperator :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.BinaryOperator
binaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionRight :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.Expression
binaryExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionWithLeft :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.BinaryExpression
binaryExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithOperator :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.BinaryOperator -> Phantoms.TTerm Query.BinaryExpression
binaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithRight :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.BinaryExpression
binaryExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryBooleanOperatorAnd :: Phantoms.TTerm Query.BinaryBooleanOperator
binaryBooleanOperatorAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

binaryBooleanOperatorOr :: Phantoms.TTerm Query.BinaryBooleanOperator
binaryBooleanOperatorOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

binaryBooleanOperatorXor :: Phantoms.TTerm Query.BinaryBooleanOperator
binaryBooleanOperatorXor =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryBooleanOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "xor"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorBoolean :: Phantoms.TTerm Query.BinaryBooleanOperator -> Phantoms.TTerm Query.BinaryOperator
binaryOperatorBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binaryOperatorComparison :: Phantoms.TTerm Query.ComparisonOperator -> Phantoms.TTerm Query.BinaryOperator
binaryOperatorComparison x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "comparison"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

binaryOperatorPower :: Phantoms.TTerm Query.BinaryOperator
binaryOperatorPower =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "power"),
        Core.fieldTerm = Core.TermUnit}}))

binding :: Phantoms.TTerm Query.Variable -> Phantoms.TTerm Query.Query -> Phantoms.TTerm Query.Binding
binding key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

bindingKey :: Phantoms.TTerm Query.Binding -> Phantoms.TTerm Query.Variable
bindingKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindingValue :: Phantoms.TTerm Query.Binding -> Phantoms.TTerm Query.Query
bindingValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

bindingWithKey :: Phantoms.TTerm Query.Binding -> Phantoms.TTerm Query.Variable -> Phantoms.TTerm Query.Binding
bindingWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

bindingWithValue :: Phantoms.TTerm Query.Binding -> Phantoms.TTerm Query.Query -> Phantoms.TTerm Query.Binding
bindingWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Binding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Binding"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

comparisonOperatorEq :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorEq =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNeq :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorNeq =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLt :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorLt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLte :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorLte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGt :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorGt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGte :: Phantoms.TTerm Query.ComparisonOperator
comparisonOperatorGte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))

edgeProjectionPattern :: Phantoms.TTerm Model.Direction -> Phantoms.TTerm (Maybe Model.EdgeLabel) -> Phantoms.TTerm [Query.PropertyPattern] -> Phantoms.TTerm (Maybe Query.VertexPattern) -> Phantoms.TTerm Query.EdgeProjectionPattern
edgeProjectionPattern direction label properties vertex =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm vertex)}]}))

edgeProjectionPatternDirection :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm Model.Direction
edgeProjectionPatternDirection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionField = (Core.Name "direction")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeProjectionPatternLabel :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm (Maybe Model.EdgeLabel)
edgeProjectionPatternLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeProjectionPatternProperties :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm [Query.PropertyPattern]
edgeProjectionPatternProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeProjectionPatternVertex :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm (Maybe Query.VertexPattern)
edgeProjectionPatternVertex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
        Core.projectionField = (Core.Name "vertex")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgeProjectionPatternWithDirection :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm Model.Direction -> Phantoms.TTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithDirection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "vertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeProjectionPatternWithLabel :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm (Maybe Model.EdgeLabel) -> Phantoms.TTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "direction")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "vertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeProjectionPatternWithProperties :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm [Query.PropertyPattern] -> Phantoms.TTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "direction")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "vertex")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgeProjectionPatternWithVertex :: Phantoms.TTerm Query.EdgeProjectionPattern -> Phantoms.TTerm (Maybe Query.VertexPattern) -> Phantoms.TTerm Query.EdgeProjectionPattern
edgeProjectionPatternWithVertex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "direction")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.EdgeProjectionPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "vertex"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

expressionAssociative :: Phantoms.TTerm Query.AssociativeExpression -> Phantoms.TTerm Query.Expression
expressionAssociative x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "associative"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBinary :: Phantoms.TTerm Query.BinaryExpression -> Phantoms.TTerm Query.Expression
expressionBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionProperty :: Phantoms.TTerm Query.PropertyProjection -> Phantoms.TTerm Query.Expression
expressionProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUnary :: Phantoms.TTerm Query.UnaryExpression -> Phantoms.TTerm Query.Expression
expressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVariable :: Phantoms.TTerm Query.Variable -> Phantoms.TTerm Query.Expression
expressionVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionVertex :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm Query.Expression
expressionVertex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "vertex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

letQuery :: Phantoms.TTerm [Query.Binding] -> Phantoms.TTerm Query.Query -> Phantoms.TTerm Query.LetQuery
letQuery bindings environment =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Phantoms.unTTerm environment)}]}))

letQueryBindings :: Phantoms.TTerm Query.LetQuery -> Phantoms.TTerm [Query.Binding]
letQueryBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letQueryEnvironment :: Phantoms.TTerm Query.LetQuery -> Phantoms.TTerm Query.Query
letQueryEnvironment x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
        Core.projectionField = (Core.Name "environment")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letQueryWithBindings :: Phantoms.TTerm Query.LetQuery -> Phantoms.TTerm [Query.Binding] -> Phantoms.TTerm Query.LetQuery
letQueryWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
              Core.projectionField = (Core.Name "environment")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letQueryWithEnvironment :: Phantoms.TTerm Query.LetQuery -> Phantoms.TTerm Query.Query -> Phantoms.TTerm Query.LetQuery
letQueryWithEnvironment original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.LetQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.LetQuery"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "environment"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

matchQuery :: Phantoms.TTerm Bool -> Phantoms.TTerm [Query.Projection] -> Phantoms.TTerm (Maybe Query.Expression) -> Phantoms.TTerm Query.MatchQuery
matchQuery optional pattern where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm optional)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)}]}))

matchQueryOptional :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm Bool
matchQueryOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionField = (Core.Name "optional")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchQueryPattern :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm [Query.Projection]
matchQueryPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionField = (Core.Name "pattern")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchQueryWhere :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm (Maybe Query.Expression)
matchQueryWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
        Core.projectionField = (Core.Name "where")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchQueryWithOptional :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm Bool -> Phantoms.TTerm Query.MatchQuery
matchQueryWithOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "where")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchQueryWithPattern :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm [Query.Projection] -> Phantoms.TTerm Query.MatchQuery
matchQueryWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "where")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchQueryWithWhere :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm (Maybe Query.Expression) -> Phantoms.TTerm Query.MatchQuery
matchQueryWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "optional")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.MatchQuery"),
              Core.projectionField = (Core.Name "pattern")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projection :: Phantoms.TTerm Query.Expression -> Phantoms.TTerm (Maybe Query.Variable) -> Phantoms.TTerm Query.Projection
projection value as =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm as)}]}))

projectionValue :: Phantoms.TTerm Query.Projection -> Phantoms.TTerm Query.Expression
projectionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionAs :: Phantoms.TTerm Query.Projection -> Phantoms.TTerm (Maybe Query.Variable)
projectionAs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
        Core.projectionField = (Core.Name "as")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionWithValue :: Phantoms.TTerm Query.Projection -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.Projection
projectionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
              Core.projectionField = (Core.Name "as")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionWithAs :: Phantoms.TTerm Query.Projection -> Phantoms.TTerm (Maybe Query.Variable) -> Phantoms.TTerm Query.Projection
projectionWithAs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projection"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "as"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projections :: Phantoms.TTerm Bool -> Phantoms.TTerm [Query.Projection] -> Phantoms.TTerm Query.Projections
projections all explicit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTTerm all)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTTerm explicit)}]}))

projectionsAll :: Phantoms.TTerm Query.Projections -> Phantoms.TTerm Bool
projectionsAll x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
        Core.projectionField = (Core.Name "all")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionsExplicit :: Phantoms.TTerm Query.Projections -> Phantoms.TTerm [Query.Projection]
projectionsExplicit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
        Core.projectionField = (Core.Name "explicit")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionsWithAll :: Phantoms.TTerm Query.Projections -> Phantoms.TTerm Bool -> Phantoms.TTerm Query.Projections
projectionsWithAll original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
              Core.projectionField = (Core.Name "explicit")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionsWithExplicit :: Phantoms.TTerm Query.Projections -> Phantoms.TTerm [Query.Projection] -> Phantoms.TTerm Query.Projections
projectionsWithExplicit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.Projections"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.Projections"),
              Core.projectionField = (Core.Name "all")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyPattern :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Query.PropertyValuePattern -> Phantoms.TTerm Query.PropertyPattern
propertyPattern key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

propertyPatternKey :: Phantoms.TTerm Query.PropertyPattern -> Phantoms.TTerm Model.PropertyKey
propertyPatternKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyPatternValue :: Phantoms.TTerm Query.PropertyPattern -> Phantoms.TTerm Query.PropertyValuePattern
propertyPatternValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
        Core.projectionField = (Core.Name "value")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyPatternWithKey :: Phantoms.TTerm Query.PropertyPattern -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Query.PropertyPattern
propertyPatternWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
              Core.projectionField = (Core.Name "value")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyPatternWithValue :: Phantoms.TTerm Query.PropertyPattern -> Phantoms.TTerm Query.PropertyValuePattern -> Phantoms.TTerm Query.PropertyPattern
propertyPatternWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyPattern"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyProjection :: Phantoms.TTerm Query.Expression -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Query.PropertyProjection
propertyProjection base key =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTTerm base)},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)}]}))

propertyProjectionBase :: Phantoms.TTerm Query.PropertyProjection -> Phantoms.TTerm Query.Expression
propertyProjectionBase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
        Core.projectionField = (Core.Name "base")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyProjectionKey :: Phantoms.TTerm Query.PropertyProjection -> Phantoms.TTerm Model.PropertyKey
propertyProjectionKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
        Core.projectionField = (Core.Name "key")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyProjectionWithBase :: Phantoms.TTerm Query.PropertyProjection -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.PropertyProjection
propertyProjectionWithBase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
              Core.projectionField = (Core.Name "key")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyProjectionWithKey :: Phantoms.TTerm Query.PropertyProjection -> Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Query.PropertyProjection
propertyProjectionWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "base"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.PropertyProjection"),
              Core.projectionField = (Core.Name "base")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyValue :: Phantoms.TTerm String -> Phantoms.TTerm Query.PropertyValue
propertyValue x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.PropertyValue"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unPropertyValue :: Phantoms.TTerm Query.PropertyValue -> Phantoms.TTerm String
unPropertyValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.query.PropertyValue")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyValuePatternVariable :: Phantoms.TTerm Model.PropertyKey -> Phantoms.TTerm Query.PropertyValuePattern
propertyValuePatternVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.PropertyValuePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyValuePatternValue :: Phantoms.TTerm String -> Phantoms.TTerm Query.PropertyValuePattern
propertyValuePatternValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.PropertyValuePattern"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryApplication :: Phantoms.TTerm Query.ApplicationQuery -> Phantoms.TTerm Query.Query
queryApplication x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "application"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryAggregate :: Phantoms.TTerm Query.AggregationQuery -> Phantoms.TTerm Query.Query
queryAggregate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "aggregate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryLetQuery :: Phantoms.TTerm Query.LetQuery -> Phantoms.TTerm Query.Query
queryLetQuery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "LetQuery"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryMatch :: Phantoms.TTerm Query.MatchQuery -> Phantoms.TTerm Query.Query
queryMatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

querySelect :: Phantoms.TTerm Query.SelectQuery -> Phantoms.TTerm Query.Query
querySelect x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "select"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryValue :: Phantoms.TTerm String -> Phantoms.TTerm Query.Query
queryValue x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "value"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

selectQuery :: Phantoms.TTerm Bool -> Phantoms.TTerm Query.Projections -> Phantoms.TTerm Query.SelectQuery
selectQuery distinct projection =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm projection)}]}))

selectQueryDistinct :: Phantoms.TTerm Query.SelectQuery -> Phantoms.TTerm Bool
selectQueryDistinct x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
        Core.projectionField = (Core.Name "distinct")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectQueryProjection :: Phantoms.TTerm Query.SelectQuery -> Phantoms.TTerm Query.Projections
selectQueryProjection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
        Core.projectionField = (Core.Name "projection")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

selectQueryWithDistinct :: Phantoms.TTerm Query.SelectQuery -> Phantoms.TTerm Bool -> Phantoms.TTerm Query.SelectQuery
selectQueryWithDistinct original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
              Core.projectionField = (Core.Name "projection")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

selectQueryWithProjection :: Phantoms.TTerm Query.SelectQuery -> Phantoms.TTerm Query.Projections -> Phantoms.TTerm Query.SelectQuery
selectQueryWithProjection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.SelectQuery"),
              Core.projectionField = (Core.Name "distinct")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryExpression :: Phantoms.TTerm Query.UnaryOperator -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.UnaryExpression
unaryExpression operator operand =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm operand)}]}))

unaryExpressionOperator :: Phantoms.TTerm Query.UnaryExpression -> Phantoms.TTerm Query.UnaryOperator
unaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
        Core.projectionField = (Core.Name "operator")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionOperand :: Phantoms.TTerm Query.UnaryExpression -> Phantoms.TTerm Query.Expression
unaryExpressionOperand x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
        Core.projectionField = (Core.Name "operand")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionWithOperator :: Phantoms.TTerm Query.UnaryExpression -> Phantoms.TTerm Query.UnaryOperator -> Phantoms.TTerm Query.UnaryExpression
unaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
              Core.projectionField = (Core.Name "operand")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryExpressionWithOperand :: Phantoms.TTerm Query.UnaryExpression -> Phantoms.TTerm Query.Expression -> Phantoms.TTerm Query.UnaryExpression
unaryExpressionWithOperand original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.UnaryExpression"),
              Core.projectionField = (Core.Name "operator")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operand"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryOperatorNegate :: Phantoms.TTerm Query.UnaryOperator
unaryOperatorNegate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.pg.query.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negate"),
        Core.fieldTerm = Core.TermUnit}}))

variable :: Phantoms.TTerm String -> Phantoms.TTerm Query.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.pg.query.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm Query.Variable -> Phantoms.TTerm String
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.pg.query.Variable")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPattern :: Phantoms.TTerm (Maybe Query.Variable) -> Phantoms.TTerm (Maybe Model.VertexLabel) -> Phantoms.TTerm [Query.PropertyPattern] -> Phantoms.TTerm [Query.EdgeProjectionPattern] -> Phantoms.TTerm Query.VertexPattern
vertexPattern variable label properties edges =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm label)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm edges)}]}))

vertexPatternVariable :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm (Maybe Query.Variable)
vertexPatternVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionField = (Core.Name "variable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPatternLabel :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm (Maybe Model.VertexLabel)
vertexPatternLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionField = (Core.Name "label")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPatternProperties :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm [Query.PropertyPattern]
vertexPatternProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionField = (Core.Name "properties")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPatternEdges :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm [Query.EdgeProjectionPattern]
vertexPatternEdges x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
        Core.projectionField = (Core.Name "edges")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

vertexPatternWithVariable :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm (Maybe Query.Variable) -> Phantoms.TTerm Query.VertexPattern
vertexPatternWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "edges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPatternWithLabel :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm (Maybe Model.VertexLabel) -> Phantoms.TTerm Query.VertexPattern
vertexPatternWithLabel original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "edges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPatternWithProperties :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm [Query.PropertyPattern] -> Phantoms.TTerm Query.VertexPattern
vertexPatternWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "edges")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

vertexPatternWithEdges :: Phantoms.TTerm Query.VertexPattern -> Phantoms.TTerm [Query.EdgeProjectionPattern] -> Phantoms.TTerm Query.VertexPattern
vertexPatternWithEdges original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "label")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.pg.query.VertexPattern"),
              Core.projectionField = (Core.Name "properties")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
