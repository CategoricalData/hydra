-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for com.gdblab.pathAlgebra.syntax

module Hydra.Dsl.Com.Gdblab.PathAlgebra.Syntax where

import qualified Com.Gdblab.PathAlgebra.Syntax as Syntax
import qualified Hydra.Core as Core
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

alternation :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Alternation
alternation left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

alternationLeft :: Phantoms.TypedTerm Syntax.Alternation -> Phantoms.TypedTerm Syntax.Rpq
alternationLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

alternationRight :: Phantoms.TypedTerm Syntax.Alternation -> Phantoms.TypedTerm Syntax.Rpq
alternationRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

alternationWithLeft :: Phantoms.TypedTerm Syntax.Alternation -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Alternation
alternationWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

alternationWithRight :: Phantoms.TypedTerm Syntax.Alternation -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Alternation
alternationWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

boolOpAnd :: Phantoms.TypedTerm Syntax.BoolOp
boolOpAnd =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.BoolOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

boolOpOr :: Phantoms.TypedTerm Syntax.BoolOp
boolOpOr =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.BoolOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymEqual :: Phantoms.TypedTerm Syntax.CompareSym
compareSymEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymGreaterThan :: Phantoms.TypedTerm Syntax.CompareSym
compareSymGreaterThan =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymGreaterThanOrEqual :: Phantoms.TypedTerm Syntax.CompareSym
compareSymGreaterThanOrEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymLessThan :: Phantoms.TypedTerm Syntax.CompareSym
compareSymLessThan =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymLessThanOrEqual :: Phantoms.TypedTerm Syntax.CompareSym
compareSymLessThanOrEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymNotEqual :: Phantoms.TypedTerm Syntax.CompareSym
compareSymNotEqual =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

complexConditionCompound :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.ComplexCondition
complexConditionCompound x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

complexConditionSimple :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.ComplexCondition
complexConditionSimple x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

complexFunction :: Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.ComplexFunction
complexFunction name innerFunction additionalArg =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm innerFunction)},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Phantoms.unTypedTerm additionalArg)}]}))

complexFunctionAdditionalArg :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Text
complexFunctionAdditionalArg x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionFieldName = (Core.Name "additionalArg")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

complexFunctionInnerFunction :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Function
complexFunctionInnerFunction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionFieldName = (Core.Name "innerFunction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

complexFunctionName :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Text
complexFunctionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

complexFunctionWithAdditionalArg :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.ComplexFunction
complexFunctionWithAdditionalArg original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

complexFunctionWithInnerFunction :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.ComplexFunction
complexFunctionWithInnerFunction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "additionalArg")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

complexFunctionWithName :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.ComplexFunction
complexFunctionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionFieldName = (Core.Name "additionalArg")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

compoundComplexCondition :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.BoolOp -> Phantoms.TypedTerm Syntax.ComplexCondition -> Phantoms.TypedTerm Syntax.CompoundComplexCondition
compoundComplexCondition lhs operator rhs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm rhs)}]}))

compoundComplexConditionLhs :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.Condition
compoundComplexConditionLhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

compoundComplexConditionOperator :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.BoolOp
compoundComplexConditionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

compoundComplexConditionRhs :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.ComplexCondition
compoundComplexConditionRhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

compoundComplexConditionWithLhs :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithLhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

compoundComplexConditionWithOperator :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.BoolOp -> Phantoms.TypedTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

compoundComplexConditionWithRhs :: Phantoms.TypedTerm Syntax.CompoundComplexCondition -> Phantoms.TypedTerm Syntax.ComplexCondition -> Phantoms.TypedTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithRhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

concatenation :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Concatenation
concatenation left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

concatenationLeft :: Phantoms.TypedTerm Syntax.Concatenation -> Phantoms.TypedTerm Syntax.Rpq
concatenationLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

concatenationRight :: Phantoms.TypedTerm Syntax.Concatenation -> Phantoms.TypedTerm Syntax.Rpq
concatenationRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

concatenationWithLeft :: Phantoms.TypedTerm Syntax.Concatenation -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Concatenation
concatenationWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

concatenationWithRight :: Phantoms.TypedTerm Syntax.Concatenation -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Concatenation
concatenationWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

condition :: Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.CompareSym -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.Condition
condition function compareSym value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTypedTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Phantoms.unTypedTerm compareSym)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

conditionCompareSym :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.CompareSym
conditionCompareSym x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionFieldName = (Core.Name "compareSym")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

conditionFunction :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.Function
conditionFunction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionFieldName = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

conditionValue :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.Text
conditionValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

conditionWithCompareSym :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.CompareSym -> Phantoms.TypedTerm Syntax.Condition
conditionWithCompareSym original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

conditionWithFunction :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.Condition
conditionWithFunction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "compareSym")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

conditionWithValue :: Phantoms.TypedTerm Syntax.Condition -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.Condition
conditionWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionFieldName = (Core.Name "compareSym")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

edgeDirectionIncoming :: Phantoms.TypedTerm Syntax.EdgeDirection
edgeDirectionIncoming =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incoming"),
        Core.fieldTerm = Core.TermUnit}}))

edgeDirectionOutgoing :: Phantoms.TypedTerm Syntax.EdgeDirection
edgeDirectionOutgoing =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outgoing"),
        Core.fieldTerm = Core.TermUnit}}))

edgeDirectionUndirected :: Phantoms.TypedTerm Syntax.EdgeDirection
edgeDirectionUndirected =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))

edgePattern :: Phantoms.TypedTerm Syntax.EdgeDirection -> Phantoms.TypedTerm (Maybe Syntax.Rpq) -> Phantoms.TypedTerm Syntax.EdgePattern
edgePattern direction rpq =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Phantoms.unTypedTerm rpq)}]}))

edgePatternDirection :: Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm Syntax.EdgeDirection
edgePatternDirection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
        Core.projectionFieldName = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgePatternRpq :: Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm (Maybe Syntax.Rpq)
edgePatternRpq x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
        Core.projectionFieldName = (Core.Name "rpq")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgePatternWithDirection :: Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm Syntax.EdgeDirection -> Phantoms.TypedTerm Syntax.EdgePattern
edgePatternWithDirection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
              Core.projectionFieldName = (Core.Name "rpq")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgePatternWithRpq :: Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm (Maybe Syntax.Rpq) -> Phantoms.TypedTerm Syntax.EdgePattern
edgePatternWithRpq original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
              Core.projectionFieldName = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

functionComplex :: Phantoms.TypedTerm Syntax.ComplexFunction -> Phantoms.TypedTerm Syntax.Function
functionComplex x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

functionNested :: Phantoms.TypedTerm Syntax.NestedFunction -> Phantoms.TypedTerm Syntax.Function
functionNested x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nested"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

functionSimple :: Phantoms.TypedTerm Syntax.SimpleFunction -> Phantoms.TypedTerm Syntax.Function
functionSimple x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

groupBy :: Phantoms.TypedTerm Syntax.GroupByOption -> Phantoms.TypedTerm Syntax.GroupBy
groupBy x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupBy"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

groupByOptionLength :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSource :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionSource =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceLength :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionSourceLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceTarget :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionSourceTarget =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTarget"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceTargetLength :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionSourceTargetLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTargetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionTarget :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionTarget =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionTargetLength :: Phantoms.TypedTerm Syntax.GroupByOption
groupByOptionTargetLength =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupProjAll :: Phantoms.TypedTerm Syntax.GroupProj
groupProjAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

groupProjLimited :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Syntax.GroupProj
groupProjLimited x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nestedFunction :: Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.NestedFunction
nestedFunction name innerFunction =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm innerFunction)}]}))

nestedFunctionInnerFunction :: Phantoms.TypedTerm Syntax.NestedFunction -> Phantoms.TypedTerm Syntax.Function
nestedFunctionInnerFunction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
        Core.projectionFieldName = (Core.Name "innerFunction")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nestedFunctionName :: Phantoms.TypedTerm Syntax.NestedFunction -> Phantoms.TypedTerm Syntax.Text
nestedFunctionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nestedFunctionWithInnerFunction :: Phantoms.TypedTerm Syntax.NestedFunction -> Phantoms.TypedTerm Syntax.Function -> Phantoms.TypedTerm Syntax.NestedFunction
nestedFunctionWithInnerFunction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nestedFunctionWithName :: Phantoms.TypedTerm Syntax.NestedFunction -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.NestedFunction
nestedFunctionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
              Core.projectionFieldName = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodePattern :: Phantoms.TypedTerm (Maybe Syntax.Variable) -> Phantoms.TypedTerm Syntax.NodePattern
nodePattern variable =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)}]}))

nodePatternVariable :: Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm (Maybe Syntax.Variable)
nodePatternVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternWithVariable :: Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm (Maybe Syntax.Variable) -> Phantoms.TypedTerm Syntax.NodePattern
nodePatternWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

orderBy :: Phantoms.TypedTerm Syntax.OrderByOption -> Phantoms.TypedTerm Syntax.OrderBy
orderBy x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderBy"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

orderByOptionGroup :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionGroup =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionGroupPath :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionGroupPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartition :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionPartition =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partition"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionGroup :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionPartitionGroup =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroup"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionGroupPath :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionPartitionGroupPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionPath :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionPartitionPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPath :: Phantoms.TypedTerm Syntax.OrderByOption
orderByOptionPath =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

partProjAll :: Phantoms.TypedTerm Syntax.PartProj
partProjAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PartProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

partProjLimited :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Syntax.PartProj
partProjLimited x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PartProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathPattern :: Phantoms.TypedTerm Syntax.PathName -> Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm (Maybe Syntax.ComplexCondition) -> Phantoms.TypedTerm Syntax.PathPattern
pathPattern pathName startNode edge endNode condition =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Phantoms.unTypedTerm pathName)},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm startNode)},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTypedTerm edge)},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm endNode)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm condition)}]}))

pathPatternCondition :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm (Maybe Syntax.ComplexCondition)
pathPatternCondition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathPatternEdge :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.EdgePattern
pathPatternEdge x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionFieldName = (Core.Name "edge")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathPatternEndNode :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.NodePattern
pathPatternEndNode x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionFieldName = (Core.Name "endNode")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathPatternPathName :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.PathName
pathPatternPathName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionFieldName = (Core.Name "pathName")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathPatternStartNode :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.NodePattern
pathPatternStartNode x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionFieldName = (Core.Name "startNode")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathPatternWithCondition :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm (Maybe Syntax.ComplexCondition) -> Phantoms.TypedTerm Syntax.PathPattern
pathPatternWithCondition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pathPatternWithEdge :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.EdgePattern -> Phantoms.TypedTerm Syntax.PathPattern
pathPatternWithEdge original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathPatternWithEndNode :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm Syntax.PathPattern
pathPatternWithEndNode original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathPatternWithPathName :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.PathName -> Phantoms.TypedTerm Syntax.PathPattern
pathPatternWithPathName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathPatternWithStartNode :: Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.NodePattern -> Phantoms.TypedTerm Syntax.PathPattern
pathPatternWithStartNode original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathProjAll :: Phantoms.TypedTerm Syntax.PathProj
pathProjAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

pathProjLimited :: Phantoms.TypedTerm Syntax.Number -> Phantoms.TypedTerm Syntax.PathProj
pathProjLimited x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

pathQuery :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm (Maybe Syntax.RestrictorExt) -> Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm (Maybe Syntax.GroupBy) -> Phantoms.TypedTerm (Maybe Syntax.OrderBy) -> Phantoms.TypedTerm Syntax.PathQuery
pathQuery projection restrictorExt pathPattern groupBy orderBy =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm projection)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Phantoms.unTypedTerm restrictorExt)},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pathPattern)},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Phantoms.unTypedTerm groupBy)},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Phantoms.unTypedTerm orderBy)}]}))

pathQueryGroupBy :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.GroupBy)
pathQueryGroupBy x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionFieldName = (Core.Name "groupBy")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathQueryOrderBy :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.OrderBy)
pathQueryOrderBy x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionFieldName = (Core.Name "orderBy")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathQueryPathPattern :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm Syntax.PathPattern
pathQueryPathPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionFieldName = (Core.Name "pathPattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathQueryProjection :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm Syntax.Projection
pathQueryProjection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionFieldName = (Core.Name "projection")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathQueryRestrictorExt :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.RestrictorExt)
pathQueryRestrictorExt x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionFieldName = (Core.Name "restrictorExt")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

pathQueryWithGroupBy :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.GroupBy) -> Phantoms.TypedTerm Syntax.PathQuery
pathQueryWithGroupBy original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathQueryWithOrderBy :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.OrderBy) -> Phantoms.TypedTerm Syntax.PathQuery
pathQueryWithOrderBy original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pathQueryWithPathPattern :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm Syntax.PathPattern -> Phantoms.TypedTerm Syntax.PathQuery
pathQueryWithPathPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathQueryWithProjection :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.PathQuery
pathQueryWithProjection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

pathQueryWithRestrictorExt :: Phantoms.TypedTerm Syntax.PathQuery -> Phantoms.TypedTerm (Maybe Syntax.RestrictorExt) -> Phantoms.TypedTerm Syntax.PathQuery
pathQueryWithRestrictorExt original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionFieldName = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

plus :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TypedTerm Syntax.Plus
plus expression restrictor =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTypedTerm restrictor)}]}))

plusExpression :: Phantoms.TypedTerm Syntax.Plus -> Phantoms.TypedTerm Syntax.Rpq
plusExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

plusRestrictor :: Phantoms.TypedTerm Syntax.Plus -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor)
plusRestrictor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
        Core.projectionFieldName = (Core.Name "restrictor")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

plusWithExpression :: Phantoms.TypedTerm Syntax.Plus -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Plus
plusWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
              Core.projectionFieldName = (Core.Name "restrictor")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

plusWithRestrictor :: Phantoms.TypedTerm Syntax.Plus -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TypedTerm Syntax.Plus
plusWithRestrictor original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projection :: Phantoms.TypedTerm Syntax.PartProj -> Phantoms.TypedTerm Syntax.GroupProj -> Phantoms.TypedTerm Syntax.PathProj -> Phantoms.TypedTerm Syntax.Projection
projection partProj groupProj pathProj =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm partProj)},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm groupProj)},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm pathProj)}]}))

projectionGroupProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.GroupProj
projectionGroupProj x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionFieldName = (Core.Name "groupProj")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionPartProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.PartProj
projectionPartProj x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionFieldName = (Core.Name "partProj")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionPathProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.PathProj
projectionPathProj x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionFieldName = (Core.Name "pathProj")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionWithGroupProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.GroupProj -> Phantoms.TypedTerm Syntax.Projection
projectionWithGroupProj original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "partProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "pathProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionWithPartProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.PartProj -> Phantoms.TypedTerm Syntax.Projection
projectionWithPartProj original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "groupProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "pathProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionWithPathProj :: Phantoms.TypedTerm Syntax.Projection -> Phantoms.TypedTerm Syntax.PathProj -> Phantoms.TypedTerm Syntax.Projection
projectionWithPathProj original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "partProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionFieldName = (Core.Name "groupProj")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

restrictorExtAcyclic :: Phantoms.TypedTerm Syntax.RestrictorExt
restrictorExtAcyclic =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "acyclic"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtShortest :: Phantoms.TypedTerm Syntax.RestrictorExt
restrictorExtShortest =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortest"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtSimple :: Phantoms.TypedTerm Syntax.RestrictorExt
restrictorExtSimple =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtTrail :: Phantoms.TypedTerm Syntax.RestrictorExt
restrictorExtTrail =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trail"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtWalk :: Phantoms.TypedTerm Syntax.RestrictorExt
restrictorExtWalk =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "walk"),
        Core.fieldTerm = Core.TermUnit}}))

rpqAlternation :: Phantoms.TypedTerm Syntax.Alternation -> Phantoms.TypedTerm Syntax.Rpq
rpqAlternation x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternation"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqConcatenation :: Phantoms.TypedTerm Syntax.Concatenation -> Phantoms.TypedTerm Syntax.Rpq
rpqConcatenation x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "concatenation"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqLabel :: Phantoms.TypedTerm Syntax.Label -> Phantoms.TypedTerm Syntax.Rpq
rpqLabel x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqNegated :: Phantoms.TypedTerm Syntax.Label -> Phantoms.TypedTerm Syntax.Rpq
rpqNegated x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negated"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqOptional :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Rpq
rpqOptional x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqParenthesis :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Rpq
rpqParenthesis x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesis"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqPlus :: Phantoms.TypedTerm Syntax.Plus -> Phantoms.TypedTerm Syntax.Rpq
rpqPlus x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqRestrictor :: Phantoms.TypedTerm Syntax.RestrictorExt -> Phantoms.TypedTerm Syntax.RpqRestrictor
rpqRestrictor x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RpqRestrictor"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

rpqReverse :: Phantoms.TypedTerm Syntax.Label -> Phantoms.TypedTerm Syntax.Rpq
rpqReverse x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reverse"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rpqStar :: Phantoms.TypedTerm Syntax.Star -> Phantoms.TypedTerm Syntax.Rpq
rpqStar x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

simpleFunction :: Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.SimpleFunction
simpleFunction name argument =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTypedTerm argument)}]}))

simpleFunctionArgument :: Phantoms.TypedTerm Syntax.SimpleFunction -> Phantoms.TypedTerm Syntax.Text
simpleFunctionArgument x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
        Core.projectionFieldName = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

simpleFunctionName :: Phantoms.TypedTerm Syntax.SimpleFunction -> Phantoms.TypedTerm Syntax.Text
simpleFunctionName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

simpleFunctionWithArgument :: Phantoms.TypedTerm Syntax.SimpleFunction -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.SimpleFunction
simpleFunctionWithArgument original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

simpleFunctionWithName :: Phantoms.TypedTerm Syntax.SimpleFunction -> Phantoms.TypedTerm Syntax.Text -> Phantoms.TypedTerm Syntax.SimpleFunction
simpleFunctionWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
              Core.projectionFieldName = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

star :: Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TypedTerm Syntax.Star
star expression restrictor =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTypedTerm restrictor)}]}))

starExpression :: Phantoms.TypedTerm Syntax.Star -> Phantoms.TypedTerm Syntax.Rpq
starExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

starRestrictor :: Phantoms.TypedTerm Syntax.Star -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor)
starRestrictor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
        Core.projectionFieldName = (Core.Name "restrictor")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

starWithExpression :: Phantoms.TypedTerm Syntax.Star -> Phantoms.TypedTerm Syntax.Rpq -> Phantoms.TypedTerm Syntax.Star
starWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
              Core.projectionFieldName = (Core.Name "restrictor")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

starWithRestrictor :: Phantoms.TypedTerm Syntax.Star -> Phantoms.TypedTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TypedTerm Syntax.Star
starWithRestrictor original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unGroupBy :: Phantoms.TypedTerm Syntax.GroupBy -> Phantoms.TypedTerm Syntax.GroupByOption
unGroupBy x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.GroupBy")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unOrderBy :: Phantoms.TypedTerm Syntax.OrderBy -> Phantoms.TypedTerm Syntax.OrderByOption
unOrderBy x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.OrderBy")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unRpqRestrictor :: Phantoms.TypedTerm Syntax.RpqRestrictor -> Phantoms.TypedTerm Syntax.RestrictorExt
unRpqRestrictor x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.RpqRestrictor")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
