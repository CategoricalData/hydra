-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.kusto.kql

module Hydra.Dsl.Kusto.Kql where

import qualified Hydra.Core as Core
import qualified Hydra.Kusto.Kql as Kql
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Int as I

betweenExpression :: Phantoms.TTerm Bool -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BetweenExpression
betweenExpression not expression lowerBound upperBound =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTTerm not)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "lowerBound"),
          Core.fieldTerm = (Phantoms.unTTerm lowerBound)},
        Core.Field {
          Core.fieldName = (Core.Name "upperBound"),
          Core.fieldTerm = (Phantoms.unTTerm upperBound)}]}))

betweenExpressionExpression :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression
betweenExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

betweenExpressionLowerBound :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression
betweenExpressionLowerBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
        Core.projectionField = (Core.Name "lowerBound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

betweenExpressionNot :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Bool
betweenExpressionNot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
        Core.projectionField = (Core.Name "not")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

betweenExpressionUpperBound :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression
betweenExpressionUpperBound x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
        Core.projectionField = (Core.Name "upperBound")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

betweenExpressionWithExpression :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BetweenExpression
betweenExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "not")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lowerBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "lowerBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "upperBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "upperBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

betweenExpressionWithLowerBound :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BetweenExpression
betweenExpressionWithLowerBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "not")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowerBound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "upperBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "upperBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

betweenExpressionWithNot :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm Kql.BetweenExpression
betweenExpressionWithNot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowerBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "lowerBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "upperBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "upperBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

betweenExpressionWithUpperBound :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BetweenExpression
betweenExpressionWithUpperBound original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "not")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lowerBound"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BetweenExpression"),
              Core.projectionField = (Core.Name "lowerBound")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "upperBound"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryExpression :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BinaryOperator -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BinaryExpression
binaryExpression left operator right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
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

binaryExpressionLeft :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.Expression
binaryExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionOperator :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.BinaryOperator
binaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionRight :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.Expression
binaryExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

binaryExpressionWithLeft :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BinaryExpression
binaryExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithOperator :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.BinaryOperator -> Phantoms.TTerm Kql.BinaryExpression
binaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

binaryExpressionWithRight :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.BinaryExpression
binaryExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.BinaryExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

binaryOperatorCaseInsensitiveEqual :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorCaseInsensitiveEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseInsensitiveEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorContains :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorContains =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contains"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorDivide :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorDivide =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorEndsWith :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorEndsWith =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endsWith"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorEqual :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreater :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorGreater =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greater"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorGreaterOrEqual :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorGreaterOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorHas :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorHas =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "has"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorHasPrefix :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorHasPrefix =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasPrefix"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorHasSuffix :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorHasSuffix =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hasSuffix"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLess :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorLess =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "less"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorLessOrEqual :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorLessOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorMatchesRegex :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorMatchesRegex =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "matchesRegex"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorMinus :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorMinus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minus"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorNotEqual :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorPlus :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorPlus =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorStartsWith :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorStartsWith =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startsWith"),
        Core.fieldTerm = Core.TermUnit}}))

binaryOperatorTimes :: Phantoms.TTerm Kql.BinaryOperator
binaryOperatorTimes =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BinaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "times"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionAgo :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionAgo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ago"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionBin :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionBin =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "bin"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionCount :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionCount =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionDcount :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionDcount =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dcount"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionEndofday :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionEndofday =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endofday"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionExtract :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionExtract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extract"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionFormat_datetime :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionFormat_datetime =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "format_datetime"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionMaterialize :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionMaterialize =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "materialize"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionNow :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionNow =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "now"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionRange :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionRange =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionStartofday :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionStartofday =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startofday"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionStrcat :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionStrcat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "strcat"),
        Core.fieldTerm = Core.TermUnit}}))

builtInFunctionTodynamic :: Phantoms.TTerm Kql.BuiltInFunction
builtInFunctionTodynamic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.BuiltInFunction"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "todynamic"),
        Core.fieldTerm = Core.TermUnit}}))

columnAlias :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ColumnAlias
columnAlias column alias =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm column)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)}]}))

columnAliasAlias :: Phantoms.TTerm Kql.ColumnAlias -> Phantoms.TTerm Kql.ColumnName
columnAliasAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
        Core.projectionField = (Core.Name "alias")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnAliasColumn :: Phantoms.TTerm Kql.ColumnAlias -> Phantoms.TTerm Kql.ColumnName
columnAliasColumn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
        Core.projectionField = (Core.Name "column")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnAliasWithAlias :: Phantoms.TTerm Kql.ColumnAlias -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ColumnAlias
columnAliasWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
              Core.projectionField = (Core.Name "column")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnAliasWithColumn :: Phantoms.TTerm Kql.ColumnAlias -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ColumnAlias
columnAliasWithColumn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAlias"),
              Core.projectionField = (Core.Name "alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnAssignment :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.ColumnAssignment
columnAssignment column expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm column)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

columnAssignmentColumn :: Phantoms.TTerm Kql.ColumnAssignment -> Phantoms.TTerm Kql.ColumnName
columnAssignmentColumn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
        Core.projectionField = (Core.Name "column")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnAssignmentExpression :: Phantoms.TTerm Kql.ColumnAssignment -> Phantoms.TTerm Kql.Expression
columnAssignmentExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

columnAssignmentWithColumn :: Phantoms.TTerm Kql.ColumnAssignment -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ColumnAssignment
columnAssignmentWithColumn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

columnAssignmentWithExpression :: Phantoms.TTerm Kql.ColumnAssignment -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.ColumnAssignment
columnAssignmentWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ColumnAssignment"),
              Core.projectionField = (Core.Name "column")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

columnName :: Phantoms.TTerm String -> Phantoms.TTerm Kql.ColumnName
columnName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.ColumnName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

columnsAll :: Phantoms.TTerm Kql.Columns
columnsAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Columns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

columnsSingle :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.Columns
columnsSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Columns"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandCount :: Phantoms.TTerm Kql.Command
commandCount =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "count"),
        Core.fieldTerm = Core.TermUnit}}))

commandDistinct :: Phantoms.TTerm [Kql.ColumnName] -> Phantoms.TTerm Kql.Command
commandDistinct x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "distinct"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandExtend :: Phantoms.TTerm [Kql.ColumnAssignment] -> Phantoms.TTerm Kql.Command
commandExtend x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "extend"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandJoin :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.Command
commandJoin x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "join"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandLimit :: Phantoms.TTerm Int -> Phantoms.TTerm Kql.Command
commandLimit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandMvexpand :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.Command
commandMvexpand x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "mvexpand"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandOrderBy :: Phantoms.TTerm [Kql.SortBy] -> Phantoms.TTerm Kql.Command
commandOrderBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "orderBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandParse :: Phantoms.TTerm Kql.ParseCommand -> Phantoms.TTerm Kql.Command
commandParse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandPrint :: Phantoms.TTerm Kql.PrintCommand -> Phantoms.TTerm Kql.Command
commandPrint x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "print"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandProject :: Phantoms.TTerm [Kql.Projection] -> Phantoms.TTerm Kql.Command
commandProject x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "project"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandProjectAway :: Phantoms.TTerm [Kql.ColumnName] -> Phantoms.TTerm Kql.Command
commandProjectAway x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projectAway"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandProjectRename :: Phantoms.TTerm [Kql.ColumnAlias] -> Phantoms.TTerm Kql.Command
commandProjectRename x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "projectRename"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandRender :: Phantoms.TTerm String -> Phantoms.TTerm Kql.Command
commandRender x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "render"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandSearch :: Phantoms.TTerm Kql.SearchCommand -> Phantoms.TTerm Kql.Command
commandSearch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "search"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandSortBy :: Phantoms.TTerm [Kql.SortBy] -> Phantoms.TTerm Kql.Command
commandSortBy x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sortBy"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandSummarize :: Phantoms.TTerm Kql.SummarizeCommand -> Phantoms.TTerm Kql.Command
commandSummarize x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "summarize"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandTake :: Phantoms.TTerm Int -> Phantoms.TTerm Kql.Command
commandTake x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "take"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandTop :: Phantoms.TTerm Kql.TopCommand -> Phantoms.TTerm Kql.Command
commandTop x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "top"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandUnion :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm Kql.Command
commandUnion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "union"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

commandWhere :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Command
commandWhere x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Command"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "where"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

datetime :: Phantoms.TTerm String -> Phantoms.TTerm Kql.Datetime
datetime x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.Datetime"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

duration :: Phantoms.TTerm Int -> Phantoms.TTerm Kql.DurationUnit -> Phantoms.TTerm Kql.Duration
duration value unit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Duration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm unit)}]}))

durationUnit :: Phantoms.TTerm Kql.Duration -> Phantoms.TTerm Kql.DurationUnit
durationUnit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Duration"),
        Core.projectionField = (Core.Name "unit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

durationUnitHour :: Phantoms.TTerm Kql.DurationUnit
durationUnitHour =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.DurationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hour"),
        Core.fieldTerm = Core.TermUnit}}))

durationUnitMinute :: Phantoms.TTerm Kql.DurationUnit
durationUnitMinute =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.DurationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "minute"),
        Core.fieldTerm = Core.TermUnit}}))

durationUnitSecond :: Phantoms.TTerm Kql.DurationUnit
durationUnitSecond =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.DurationUnit"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "second"),
        Core.fieldTerm = Core.TermUnit}}))

durationValue :: Phantoms.TTerm Kql.Duration -> Phantoms.TTerm Int
durationValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Duration"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

durationWithUnit :: Phantoms.TTerm Kql.Duration -> Phantoms.TTerm Kql.DurationUnit -> Phantoms.TTerm Kql.Duration
durationWithUnit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Duration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Duration"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

durationWithValue :: Phantoms.TTerm Kql.Duration -> Phantoms.TTerm Int -> Phantoms.TTerm Kql.Duration
durationWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Duration"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "unit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Duration"),
              Core.projectionField = (Core.Name "unit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

expressionAnd :: Phantoms.TTerm [Kql.Expression] -> Phantoms.TTerm Kql.Expression
expressionAnd x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionAny :: Phantoms.TTerm Kql.Expression
expressionAny =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

expressionBetween :: Phantoms.TTerm Kql.BetweenExpression -> Phantoms.TTerm Kql.Expression
expressionBetween x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "between"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBinary :: Phantoms.TTerm Kql.BinaryExpression -> Phantoms.TTerm Kql.Expression
expressionBinary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "binary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionBraces :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Expression
expressionBraces x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "braces"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionColumn :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.Expression
expressionColumn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "column"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionDataset :: Phantoms.TTerm Kql.TableName -> Phantoms.TTerm Kql.Expression
expressionDataset x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dataset"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionIndex :: Phantoms.TTerm Kql.IndexExpression -> Phantoms.TTerm Kql.Expression
expressionIndex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "index"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionList :: Phantoms.TTerm [Kql.Expression] -> Phantoms.TTerm Kql.Expression
expressionList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionLiteral :: Phantoms.TTerm Kql.Literal -> Phantoms.TTerm Kql.Expression
expressionLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionOr :: Phantoms.TTerm [Kql.Expression] -> Phantoms.TTerm Kql.Expression
expressionOr x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionParentheses :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Expression
expressionParentheses x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parentheses"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionProperty :: Phantoms.TTerm Kql.PropertyExpression -> Phantoms.TTerm Kql.Expression
expressionProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

expressionUnary :: Phantoms.TTerm Kql.UnaryExpression -> Phantoms.TTerm Kql.Expression
expressionUnary x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Expression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unary"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionBuiltIn :: Phantoms.TTerm Kql.BuiltInFunction -> Phantoms.TTerm Kql.Function
functionBuiltIn x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "builtIn"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionCustom :: Phantoms.TTerm Kql.FunctionName -> Phantoms.TTerm Kql.Function
functionCustom x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "custom"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionExpression :: Phantoms.TTerm Kql.Function -> Phantoms.TTerm [Kql.Expression] -> Phantoms.TTerm Kql.FunctionExpression
functionExpression function arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

functionExpressionArguments :: Phantoms.TTerm Kql.FunctionExpression -> Phantoms.TTerm [Kql.Expression]
functionExpressionArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionFunction :: Phantoms.TTerm Kql.FunctionExpression -> Phantoms.TTerm Kql.Function
functionExpressionFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
        Core.projectionField = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionExpressionWithArguments :: Phantoms.TTerm Kql.FunctionExpression -> Phantoms.TTerm [Kql.Expression] -> Phantoms.TTerm Kql.FunctionExpression
functionExpressionWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
              Core.projectionField = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionExpressionWithFunction :: Phantoms.TTerm Kql.FunctionExpression -> Phantoms.TTerm Kql.Function -> Phantoms.TTerm Kql.FunctionExpression
functionExpressionWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.FunctionExpression"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionName :: Phantoms.TTerm String -> Phantoms.TTerm Kql.FunctionName
functionName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.FunctionName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

indexExpression :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm String -> Phantoms.TTerm Kql.IndexExpression
indexExpression expression index =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm index)}]}))

indexExpressionExpression :: Phantoms.TTerm Kql.IndexExpression -> Phantoms.TTerm Kql.Expression
indexExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indexExpressionIndex :: Phantoms.TTerm Kql.IndexExpression -> Phantoms.TTerm String
indexExpressionIndex x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
        Core.projectionField = (Core.Name "index")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

indexExpressionWithExpression :: Phantoms.TTerm Kql.IndexExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.IndexExpression
indexExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
              Core.projectionField = (Core.Name "index")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

indexExpressionWithIndex :: Phantoms.TTerm Kql.IndexExpression -> Phantoms.TTerm String -> Phantoms.TTerm Kql.IndexExpression
indexExpressionWithIndex original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.IndexExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "index"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

joinCommand :: Phantoms.TTerm Kql.JoinKind -> Phantoms.TTerm Kql.TableName -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.JoinCommand
joinCommand kind expression on =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "on"),
          Core.fieldTerm = (Phantoms.unTTerm on)}]}))

joinCommandExpression :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.TableName
joinCommandExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinCommandKind :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.JoinKind
joinCommandKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
        Core.projectionField = (Core.Name "kind")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinCommandOn :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.Expression
joinCommandOn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
        Core.projectionField = (Core.Name "on")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinCommandWithExpression :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.TableName -> Phantoms.TTerm Kql.JoinCommand
joinCommandWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "on"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "on")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

joinCommandWithKind :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.JoinKind -> Phantoms.TTerm Kql.JoinCommand
joinCommandWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "on"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "on")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

joinCommandWithOn :: Phantoms.TTerm Kql.JoinCommand -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.JoinCommand
joinCommandWithOn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.JoinCommand"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "on"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

joinKindFullouter :: Phantoms.TTerm Kql.JoinKind
joinKindFullouter =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "fullouter"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindInner :: Phantoms.TTerm Kql.JoinKind
joinKindInner =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inner"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindInnerunique :: Phantoms.TTerm Kql.JoinKind
joinKindInnerunique =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "innerunique"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindLeftanti :: Phantoms.TTerm Kql.JoinKind
joinKindLeftanti =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftanti"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindLeftouter :: Phantoms.TTerm Kql.JoinKind
joinKindLeftouter =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftouter"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindLeftsemi :: Phantoms.TTerm Kql.JoinKind
joinKindLeftsemi =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "leftsemi"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindRightanti :: Phantoms.TTerm Kql.JoinKind
joinKindRightanti =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightanti"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindRightouter :: Phantoms.TTerm Kql.JoinKind
joinKindRightouter =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightouter"),
        Core.fieldTerm = Core.TermUnit}}))

joinKindRightsemi :: Phantoms.TTerm Kql.JoinKind
joinKindRightsemi =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.JoinKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rightsemi"),
        Core.fieldTerm = Core.TermUnit}}))

keyValuePair :: Phantoms.TTerm String -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.KeyValuePair
keyValuePair key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

keyValuePairKey :: Phantoms.TTerm Kql.KeyValuePair -> Phantoms.TTerm String
keyValuePairKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValuePairValue :: Phantoms.TTerm Kql.KeyValuePair -> Phantoms.TTerm Kql.Expression
keyValuePairValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValuePairWithKey :: Phantoms.TTerm Kql.KeyValuePair -> Phantoms.TTerm String -> Phantoms.TTerm Kql.KeyValuePair
keyValuePairWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

keyValuePairWithValue :: Phantoms.TTerm Kql.KeyValuePair -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.KeyValuePair
keyValuePairWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.KeyValuePair"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letBinding :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.LetBinding
letBinding name expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

letBindingExpression :: Phantoms.TTerm Kql.LetBinding -> Phantoms.TTerm Kql.Expression
letBindingExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBindingName :: Phantoms.TTerm Kql.LetBinding -> Phantoms.TTerm Kql.ColumnName
letBindingName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letBindingWithExpression :: Phantoms.TTerm Kql.LetBinding -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.LetBinding
letBindingWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

letBindingWithName :: Phantoms.TTerm Kql.LetBinding -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.LetBinding
letBindingWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetBinding"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpression :: Phantoms.TTerm [Kql.LetBinding] -> Phantoms.TTerm Kql.TabularExpression -> Phantoms.TTerm Kql.LetExpression
letExpression bindings expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

letExpressionBindings :: Phantoms.TTerm Kql.LetExpression -> Phantoms.TTerm [Kql.LetBinding]
letExpressionBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
        Core.projectionField = (Core.Name "bindings")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionExpression :: Phantoms.TTerm Kql.LetExpression -> Phantoms.TTerm Kql.TabularExpression
letExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

letExpressionWithBindings :: Phantoms.TTerm Kql.LetExpression -> Phantoms.TTerm [Kql.LetBinding] -> Phantoms.TTerm Kql.LetExpression
letExpressionWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

letExpressionWithExpression :: Phantoms.TTerm Kql.LetExpression -> Phantoms.TTerm Kql.TabularExpression -> Phantoms.TTerm Kql.LetExpression
letExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.LetExpression"),
              Core.projectionField = (Core.Name "bindings")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Kql.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalDatetime :: Phantoms.TTerm Kql.Datetime -> Phantoms.TTerm Kql.Literal
literalDatetime x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "datetime"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalDouble :: Phantoms.TTerm Double -> Phantoms.TTerm Kql.Literal
literalDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalDuration :: Phantoms.TTerm Kql.Duration -> Phantoms.TTerm Kql.Literal
literalDuration x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duration"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalInt :: Phantoms.TTerm Int -> Phantoms.TTerm Kql.Literal
literalInt x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalLong :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Kql.Literal
literalLong x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm String -> Phantoms.TTerm Kql.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

orderAscending :: Phantoms.TTerm Kql.Order
orderAscending =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Order"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascending"),
        Core.fieldTerm = Core.TermUnit}}))

orderDescending :: Phantoms.TTerm Kql.Order
orderDescending =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.Order"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "descending"),
        Core.fieldTerm = Core.TermUnit}}))

parameter :: Phantoms.TTerm String -> Phantoms.TTerm Kql.Literal -> Phantoms.TTerm Kql.Parameter
parameter key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

parameterKey :: Phantoms.TTerm Kql.Parameter -> Phantoms.TTerm String
parameterKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterValue :: Phantoms.TTerm Kql.Parameter -> Phantoms.TTerm Kql.Literal
parameterValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parameterWithKey :: Phantoms.TTerm Kql.Parameter -> Phantoms.TTerm String -> Phantoms.TTerm Kql.Parameter
parameterWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parameterWithValue :: Phantoms.TTerm Kql.Parameter -> Phantoms.TTerm Kql.Literal -> Phantoms.TTerm Kql.Parameter
parameterWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Parameter"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parseCommand :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm [Kql.KeyValuePair] -> Phantoms.TTerm Kql.ParseCommand
parseCommand column pairs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm column)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm pairs)}]}))

parseCommandColumn :: Phantoms.TTerm Kql.ParseCommand -> Phantoms.TTerm Kql.ColumnName
parseCommandColumn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
        Core.projectionField = (Core.Name "column")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseCommandPairs :: Phantoms.TTerm Kql.ParseCommand -> Phantoms.TTerm [Kql.KeyValuePair]
parseCommandPairs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
        Core.projectionField = (Core.Name "pairs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parseCommandWithColumn :: Phantoms.TTerm Kql.ParseCommand -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.ParseCommand
parseCommandWithColumn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
              Core.projectionField = (Core.Name "pairs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parseCommandWithPairs :: Phantoms.TTerm Kql.ParseCommand -> Phantoms.TTerm [Kql.KeyValuePair] -> Phantoms.TTerm Kql.ParseCommand
parseCommandWithPairs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.ParseCommand"),
              Core.projectionField = (Core.Name "column")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pairs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pipelineExpression :: Phantoms.TTerm [Kql.TabularExpression] -> Phantoms.TTerm Kql.PipelineExpression
pipelineExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.PipelineExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

printCommand :: Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.PrintCommand
printCommand column expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm column)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

printCommandColumn :: Phantoms.TTerm Kql.PrintCommand -> Phantoms.TTerm (Maybe Kql.ColumnName)
printCommandColumn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
        Core.projectionField = (Core.Name "column")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

printCommandExpression :: Phantoms.TTerm Kql.PrintCommand -> Phantoms.TTerm Kql.Expression
printCommandExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

printCommandWithColumn :: Phantoms.TTerm Kql.PrintCommand -> Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm Kql.PrintCommand
printCommandWithColumn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

printCommandWithExpression :: Phantoms.TTerm Kql.PrintCommand -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.PrintCommand
printCommandWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PrintCommand"),
              Core.projectionField = (Core.Name "column")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projection :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm Kql.Projection
projection expression alias =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm alias)}]}))

projectionAlias :: Phantoms.TTerm Kql.Projection -> Phantoms.TTerm (Maybe Kql.ColumnName)
projectionAlias x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Projection"),
        Core.projectionField = (Core.Name "alias")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionExpression :: Phantoms.TTerm Kql.Projection -> Phantoms.TTerm Kql.Expression
projectionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Projection"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionWithAlias :: Phantoms.TTerm Kql.Projection -> Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm Kql.Projection
projectionWithAlias original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Projection"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionWithExpression :: Phantoms.TTerm Kql.Projection -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.Projection
projectionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alias"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.Projection"),
              Core.projectionField = (Core.Name "alias")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyExpression :: Phantoms.TTerm Kql.Expression -> Phantoms.TTerm String -> Phantoms.TTerm Kql.PropertyExpression
propertyExpression expression property =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm property)}]}))

propertyExpressionExpression :: Phantoms.TTerm Kql.PropertyExpression -> Phantoms.TTerm Kql.Expression
propertyExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExpressionProperty :: Phantoms.TTerm Kql.PropertyExpression -> Phantoms.TTerm String
propertyExpressionProperty x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
        Core.projectionField = (Core.Name "property")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExpressionWithExpression :: Phantoms.TTerm Kql.PropertyExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.PropertyExpression
propertyExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
              Core.projectionField = (Core.Name "property")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyExpressionWithProperty :: Phantoms.TTerm Kql.PropertyExpression -> Phantoms.TTerm String -> Phantoms.TTerm Kql.PropertyExpression
propertyExpressionWithProperty original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.PropertyExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "property"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

query :: Phantoms.TTerm Kql.TabularExpression -> Phantoms.TTerm Kql.Query
query x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.Query"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

searchCommand :: Phantoms.TTerm [Kql.TableName] -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.SearchCommand
searchCommand datasets pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datasets"),
          Core.fieldTerm = (Phantoms.unTTerm datasets)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

searchCommandDatasets :: Phantoms.TTerm Kql.SearchCommand -> Phantoms.TTerm [Kql.TableName]
searchCommandDatasets x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
        Core.projectionField = (Core.Name "datasets")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

searchCommandPattern :: Phantoms.TTerm Kql.SearchCommand -> Phantoms.TTerm Kql.Expression
searchCommandPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

searchCommandWithDatasets :: Phantoms.TTerm Kql.SearchCommand -> Phantoms.TTerm [Kql.TableName] -> Phantoms.TTerm Kql.SearchCommand
searchCommandWithDatasets original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datasets"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

searchCommandWithPattern :: Phantoms.TTerm Kql.SearchCommand -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.SearchCommand
searchCommandWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "datasets"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SearchCommand"),
              Core.projectionField = (Core.Name "datasets")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sortBy :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm (Maybe Kql.Order) -> Phantoms.TTerm Kql.SortBy
sortBy column order =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm column)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm order)}]}))

sortByColumn :: Phantoms.TTerm Kql.SortBy -> Phantoms.TTerm Kql.ColumnName
sortByColumn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
        Core.projectionField = (Core.Name "column")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortByOrder :: Phantoms.TTerm Kql.SortBy -> Phantoms.TTerm (Maybe Kql.Order)
sortByOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortByWithColumn :: Phantoms.TTerm Kql.SortBy -> Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm Kql.SortBy
sortByWithColumn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sortByWithOrder :: Phantoms.TTerm Kql.SortBy -> Phantoms.TTerm (Maybe Kql.Order) -> Phantoms.TTerm Kql.SortBy
sortByWithOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "column"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SortBy"),
              Core.projectionField = (Core.Name "column")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

summarizeCommand :: Phantoms.TTerm [Kql.ColumnAssignment] -> Phantoms.TTerm [Kql.ColumnName] -> Phantoms.TTerm Kql.SummarizeCommand
summarizeCommand columns by =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm columns)},
        Core.Field {
          Core.fieldName = (Core.Name "by"),
          Core.fieldTerm = (Phantoms.unTTerm by)}]}))

summarizeCommandBy :: Phantoms.TTerm Kql.SummarizeCommand -> Phantoms.TTerm [Kql.ColumnName]
summarizeCommandBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
        Core.projectionField = (Core.Name "by")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

summarizeCommandColumns :: Phantoms.TTerm Kql.SummarizeCommand -> Phantoms.TTerm [Kql.ColumnAssignment]
summarizeCommandColumns x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
        Core.projectionField = (Core.Name "columns")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

summarizeCommandWithBy :: Phantoms.TTerm Kql.SummarizeCommand -> Phantoms.TTerm [Kql.ColumnName] -> Phantoms.TTerm Kql.SummarizeCommand
summarizeCommandWithBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
              Core.projectionField = (Core.Name "columns")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "by"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

summarizeCommandWithColumns :: Phantoms.TTerm Kql.SummarizeCommand -> Phantoms.TTerm [Kql.ColumnAssignment] -> Phantoms.TTerm Kql.SummarizeCommand
summarizeCommandWithColumns original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "columns"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "by"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.SummarizeCommand"),
              Core.projectionField = (Core.Name "by")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

tableName :: Phantoms.TTerm String -> Phantoms.TTerm Kql.TableName
tableName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.kusto.kql.TableName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

tabularExpressionCommand :: Phantoms.TTerm Kql.Command -> Phantoms.TTerm Kql.TabularExpression
tabularExpressionCommand x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.TabularExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "command"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tabularExpressionLet :: Phantoms.TTerm Kql.LetExpression -> Phantoms.TTerm Kql.TabularExpression
tabularExpressionLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.TabularExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "let"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tabularExpressionPipeline :: Phantoms.TTerm Kql.PipelineExpression -> Phantoms.TTerm Kql.TabularExpression
tabularExpressionPipeline x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.TabularExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pipeline"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

tabularExpressionTable :: Phantoms.TTerm Kql.TableName -> Phantoms.TTerm Kql.TabularExpression
tabularExpressionTable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.TabularExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "table"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

topCommand :: Phantoms.TTerm Int -> Phantoms.TTerm [Kql.SortBy] -> Phantoms.TTerm Kql.TopCommand
topCommand count sort =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Phantoms.unTTerm count)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTTerm sort)}]}))

topCommandCount :: Phantoms.TTerm Kql.TopCommand -> Phantoms.TTerm Int
topCommandCount x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
        Core.projectionField = (Core.Name "count")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topCommandSort :: Phantoms.TTerm Kql.TopCommand -> Phantoms.TTerm [Kql.SortBy]
topCommandSort x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
        Core.projectionField = (Core.Name "sort")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topCommandWithCount :: Phantoms.TTerm Kql.TopCommand -> Phantoms.TTerm Int -> Phantoms.TTerm Kql.TopCommand
topCommandWithCount original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
              Core.projectionField = (Core.Name "sort")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topCommandWithSort :: Phantoms.TTerm Kql.TopCommand -> Phantoms.TTerm [Kql.SortBy] -> Phantoms.TTerm Kql.TopCommand
topCommandWithSort original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "count"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.TopCommand"),
              Core.projectionField = (Core.Name "count")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "sort"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unColumnName :: Phantoms.TTerm Kql.ColumnName -> Phantoms.TTerm String
unColumnName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.ColumnName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDatetime :: Phantoms.TTerm Kql.Datetime -> Phantoms.TTerm String
unDatetime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.Datetime")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFunctionName :: Phantoms.TTerm Kql.FunctionName -> Phantoms.TTerm String
unFunctionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.FunctionName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPipelineExpression :: Phantoms.TTerm Kql.PipelineExpression -> Phantoms.TTerm [Kql.TabularExpression]
unPipelineExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.PipelineExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unQuery :: Phantoms.TTerm Kql.Query -> Phantoms.TTerm Kql.TabularExpression
unQuery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.Query")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTableName :: Phantoms.TTerm Kql.TableName -> Phantoms.TTerm String
unTableName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.kusto.kql.TableName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpression :: Phantoms.TTerm Kql.UnaryOperator -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.UnaryExpression
unaryExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

unaryExpressionExpression :: Phantoms.TTerm Kql.UnaryExpression -> Phantoms.TTerm Kql.Expression
unaryExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionOperator :: Phantoms.TTerm Kql.UnaryExpression -> Phantoms.TTerm Kql.UnaryOperator
unaryExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryExpressionWithExpression :: Phantoms.TTerm Kql.UnaryExpression -> Phantoms.TTerm Kql.Expression -> Phantoms.TTerm Kql.UnaryExpression
unaryExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryExpressionWithOperator :: Phantoms.TTerm Kql.UnaryExpression -> Phantoms.TTerm Kql.UnaryOperator -> Phantoms.TTerm Kql.UnaryExpression
unaryExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnaryExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unaryOperatorNot :: Phantoms.TTerm Kql.UnaryOperator
unaryOperatorNot =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.UnaryOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "not"),
        Core.fieldTerm = Core.TermUnit}}))

unionCommand :: Phantoms.TTerm [Kql.Parameter] -> Phantoms.TTerm (Maybe Kql.UnionKind) -> Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm [Kql.TableName] -> Phantoms.TTerm Kql.UnionCommand
unionCommand parameters kind withSource isFuzzy tables =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm parameters)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm kind)},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Phantoms.unTTerm withSource)},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Phantoms.unTTerm isFuzzy)},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Phantoms.unTTerm tables)}]}))

unionCommandIsFuzzy :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Bool)
unionCommandIsFuzzy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
        Core.projectionField = (Core.Name "isFuzzy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionCommandKind :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Kql.UnionKind)
unionCommandKind x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
        Core.projectionField = (Core.Name "kind")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionCommandParameters :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm [Kql.Parameter]
unionCommandParameters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
        Core.projectionField = (Core.Name "parameters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionCommandTables :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm [Kql.TableName]
unionCommandTables x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
        Core.projectionField = (Core.Name "tables")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionCommandWithIsFuzzy :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Bool) -> Phantoms.TTerm Kql.UnionCommand
unionCommandWithIsFuzzy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "withSource")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "tables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionCommandWithKind :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Kql.UnionKind) -> Phantoms.TTerm Kql.UnionCommand
unionCommandWithKind original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "withSource")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "isFuzzy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "tables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionCommandWithParameters :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm [Kql.Parameter] -> Phantoms.TTerm Kql.UnionCommand
unionCommandWithParameters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "withSource")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "isFuzzy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "tables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionCommandWithSource :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Kql.ColumnName)
unionCommandWithSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
        Core.projectionField = (Core.Name "withSource")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionCommandWithTables :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm [Kql.TableName] -> Phantoms.TTerm Kql.UnionCommand
unionCommandWithTables original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "withSource")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "isFuzzy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unionCommandWithWithSource :: Phantoms.TTerm Kql.UnionCommand -> Phantoms.TTerm (Maybe Kql.ColumnName) -> Phantoms.TTerm Kql.UnionCommand
unionCommandWithWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "parameters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "parameters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "kind"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "kind")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "withSource"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isFuzzy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "isFuzzy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tables"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.kusto.kql.UnionCommand"),
              Core.projectionField = (Core.Name "tables")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionKindInner :: Phantoms.TTerm Kql.UnionKind
unionKindInner =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.UnionKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inner"),
        Core.fieldTerm = Core.TermUnit}}))

unionKindOuter :: Phantoms.TTerm Kql.UnionKind
unionKindOuter =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.kusto.kql.UnionKind"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outer"),
        Core.fieldTerm = Core.TermUnit}}))
