-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for com.gdblab.pathAlgebra.syntax

module Hydra.Dsl.Com.Gdblab.PathAlgebra.Syntax where

import qualified Com.Gdblab.PathAlgebra.Syntax as Syntax
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

alternation :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Alternation
alternation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

alternationLeft :: Phantoms.TTerm Syntax.Alternation -> Phantoms.TTerm Syntax.Rpq
alternationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternationRight :: Phantoms.TTerm Syntax.Alternation -> Phantoms.TTerm Syntax.Rpq
alternationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alternationWithLeft :: Phantoms.TTerm Syntax.Alternation -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Alternation
alternationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alternationWithRight :: Phantoms.TTerm Syntax.Alternation -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Alternation
alternationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Alternation"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

boolOpAnd :: Phantoms.TTerm Syntax.BoolOp
boolOpAnd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.BoolOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "and"),
        Core.fieldTerm = Core.TermUnit}}))

boolOpOr :: Phantoms.TTerm Syntax.BoolOp
boolOpOr =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.BoolOp"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "or"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymEqual :: Phantoms.TTerm Syntax.CompareSym
compareSymEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equal"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymGreaterThan :: Phantoms.TTerm Syntax.CompareSym
compareSymGreaterThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThan"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymGreaterThanOrEqual :: Phantoms.TTerm Syntax.CompareSym
compareSymGreaterThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "greaterThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymLessThan :: Phantoms.TTerm Syntax.CompareSym
compareSymLessThan =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThan"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymLessThanOrEqual :: Phantoms.TTerm Syntax.CompareSym
compareSymLessThanOrEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lessThanOrEqual"),
        Core.fieldTerm = Core.TermUnit}}))

compareSymNotEqual :: Phantoms.TTerm Syntax.CompareSym
compareSymNotEqual =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompareSym"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "notEqual"),
        Core.fieldTerm = Core.TermUnit}}))

complexConditionCompound :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.ComplexCondition
complexConditionCompound x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "compound"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

complexConditionSimple :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.ComplexCondition
complexConditionSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexCondition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

complexFunction :: Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.ComplexFunction
complexFunction name innerFunction additionalArg =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTTerm innerFunction)},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Phantoms.unTTerm additionalArg)}]}))

complexFunctionAdditionalArg :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Text
complexFunctionAdditionalArg x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionField = (Core.Name "additionalArg")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

complexFunctionInnerFunction :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Function
complexFunctionInnerFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionField = (Core.Name "innerFunction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

complexFunctionName :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Text
complexFunctionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

complexFunctionWithAdditionalArg :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.ComplexFunction
complexFunctionWithAdditionalArg original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

complexFunctionWithInnerFunction :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.ComplexFunction
complexFunctionWithInnerFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "additionalArg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

complexFunctionWithName :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.ComplexFunction
complexFunctionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "additionalArg"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.ComplexFunction"),
              Core.projectionField = (Core.Name "additionalArg")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundComplexCondition :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.BoolOp -> Phantoms.TTerm Syntax.ComplexCondition -> Phantoms.TTerm Syntax.CompoundComplexCondition
compoundComplexCondition lhs operator rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

compoundComplexConditionLhs :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.Condition
compoundComplexConditionLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundComplexConditionOperator :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.BoolOp
compoundComplexConditionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundComplexConditionRhs :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.ComplexCondition
compoundComplexConditionRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

compoundComplexConditionWithLhs :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundComplexConditionWithOperator :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.BoolOp -> Phantoms.TTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

compoundComplexConditionWithRhs :: Phantoms.TTerm Syntax.CompoundComplexCondition -> Phantoms.TTerm Syntax.ComplexCondition -> Phantoms.TTerm Syntax.CompoundComplexCondition
compoundComplexConditionWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.CompoundComplexCondition"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

concatenation :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Concatenation
concatenation left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

concatenationLeft :: Phantoms.TTerm Syntax.Concatenation -> Phantoms.TTerm Syntax.Rpq
concatenationLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

concatenationRight :: Phantoms.TTerm Syntax.Concatenation -> Phantoms.TTerm Syntax.Rpq
concatenationRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

concatenationWithLeft :: Phantoms.TTerm Syntax.Concatenation -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Concatenation
concatenationWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

concatenationWithRight :: Phantoms.TTerm Syntax.Concatenation -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Concatenation
concatenationWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Concatenation"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

condition :: Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.CompareSym -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.Condition
condition function compareSym value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm function)},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Phantoms.unTTerm compareSym)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

conditionCompareSym :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.CompareSym
conditionCompareSym x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionField = (Core.Name "compareSym")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionFunction :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.Function
conditionFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionField = (Core.Name "function")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionValue :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.Text
conditionValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

conditionWithCompareSym :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.CompareSym -> Phantoms.TTerm Syntax.Condition
conditionWithCompareSym original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionWithFunction :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.Condition
conditionWithFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "compareSym")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

conditionWithValue :: Phantoms.TTerm Syntax.Condition -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.Condition
conditionWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "function"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "function")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "compareSym"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Condition"),
              Core.projectionField = (Core.Name "compareSym")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

edgeDirectionIncoming :: Phantoms.TTerm Syntax.EdgeDirection
edgeDirectionIncoming =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "incoming"),
        Core.fieldTerm = Core.TermUnit}}))

edgeDirectionOutgoing :: Phantoms.TTerm Syntax.EdgeDirection
edgeDirectionOutgoing =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "outgoing"),
        Core.fieldTerm = Core.TermUnit}}))

edgeDirectionUndirected :: Phantoms.TTerm Syntax.EdgeDirection
edgeDirectionUndirected =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgeDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "undirected"),
        Core.fieldTerm = Core.TermUnit}}))

edgePattern :: Phantoms.TTerm Syntax.EdgeDirection -> Phantoms.TTerm (Maybe Syntax.Rpq) -> Phantoms.TTerm Syntax.EdgePattern
edgePattern direction rpq =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm direction)},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Phantoms.unTTerm rpq)}]}))

edgePatternDirection :: Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm Syntax.EdgeDirection
edgePatternDirection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
        Core.projectionField = (Core.Name "direction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgePatternRpq :: Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm (Maybe Syntax.Rpq)
edgePatternRpq x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
        Core.projectionField = (Core.Name "rpq")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

edgePatternWithDirection :: Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm Syntax.EdgeDirection -> Phantoms.TTerm Syntax.EdgePattern
edgePatternWithDirection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
              Core.projectionField = (Core.Name "rpq")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

edgePatternWithRpq :: Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm (Maybe Syntax.Rpq) -> Phantoms.TTerm Syntax.EdgePattern
edgePatternWithRpq original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "direction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.EdgePattern"),
              Core.projectionField = (Core.Name "direction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rpq"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionComplex :: Phantoms.TTerm Syntax.ComplexFunction -> Phantoms.TTerm Syntax.Function
functionComplex x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "complex"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionNested :: Phantoms.TTerm Syntax.NestedFunction -> Phantoms.TTerm Syntax.Function
functionNested x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nested"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

functionSimple :: Phantoms.TTerm Syntax.SimpleFunction -> Phantoms.TTerm Syntax.Function
functionSimple x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Function"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

groupBy :: Phantoms.TTerm Syntax.GroupByOption -> Phantoms.TTerm Syntax.GroupBy
groupBy x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupBy"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

groupByOptionLength :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "length"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSource :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionSource =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "source"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceLength :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionSourceLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceTarget :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionSourceTarget =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTarget"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionSourceTargetLength :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionSourceTargetLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sourceTargetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionTarget :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionTarget =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "target"),
        Core.fieldTerm = Core.TermUnit}}))

groupByOptionTargetLength :: Phantoms.TTerm Syntax.GroupByOption
groupByOptionTargetLength =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "targetLength"),
        Core.fieldTerm = Core.TermUnit}}))

groupProjAll :: Phantoms.TTerm Syntax.GroupProj
groupProjAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

groupProjLimited :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.GroupProj
groupProjLimited x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.GroupProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

nestedFunction :: Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.NestedFunction
nestedFunction name innerFunction =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTTerm innerFunction)}]}))

nestedFunctionInnerFunction :: Phantoms.TTerm Syntax.NestedFunction -> Phantoms.TTerm Syntax.Function
nestedFunctionInnerFunction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
        Core.projectionField = (Core.Name "innerFunction")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedFunctionName :: Phantoms.TTerm Syntax.NestedFunction -> Phantoms.TTerm Syntax.Text
nestedFunctionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nestedFunctionWithInnerFunction :: Phantoms.TTerm Syntax.NestedFunction -> Phantoms.TTerm Syntax.Function -> Phantoms.TTerm Syntax.NestedFunction
nestedFunctionWithInnerFunction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nestedFunctionWithName :: Phantoms.TTerm Syntax.NestedFunction -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.NestedFunction
nestedFunctionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "innerFunction"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NestedFunction"),
              Core.projectionField = (Core.Name "innerFunction")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodePattern :: Phantoms.TTerm (Maybe Syntax.Variable) -> Phantoms.TTerm Syntax.NodePattern
nodePattern variable =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)}]}))

nodePatternVariable :: Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm (Maybe Syntax.Variable)
nodePatternVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternWithVariable :: Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm (Maybe Syntax.Variable) -> Phantoms.TTerm Syntax.NodePattern
nodePatternWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

orderBy :: Phantoms.TTerm Syntax.OrderByOption -> Phantoms.TTerm Syntax.OrderBy
orderBy x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderBy"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

orderByOptionGroup :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionGroup =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "group"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionGroupPath :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionGroupPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "groupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartition :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionPartition =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partition"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionGroup :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionPartitionGroup =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroup"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionGroupPath :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionPartitionGroupPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionGroupPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPartitionPath :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionPartitionPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partitionPath"),
        Core.fieldTerm = Core.TermUnit}}))

orderByOptionPath :: Phantoms.TTerm Syntax.OrderByOption
orderByOptionPath =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.OrderByOption"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "path"),
        Core.fieldTerm = Core.TermUnit}}))

partProjAll :: Phantoms.TTerm Syntax.PartProj
partProjAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PartProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

partProjLimited :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.PartProj
partProjLimited x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PartProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathPattern :: Phantoms.TTerm Syntax.PathName -> Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm (Maybe Syntax.ComplexCondition) -> Phantoms.TTerm Syntax.PathPattern
pathPattern pathName startNode edge endNode condition =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Phantoms.unTTerm pathName)},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Phantoms.unTTerm startNode)},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTTerm edge)},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Phantoms.unTTerm endNode)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)}]}))

pathPatternCondition :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm (Maybe Syntax.ComplexCondition)
pathPatternCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathPatternEdge :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.EdgePattern
pathPatternEdge x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionField = (Core.Name "edge")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathPatternEndNode :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.NodePattern
pathPatternEndNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionField = (Core.Name "endNode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathPatternPathName :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.PathName
pathPatternPathName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionField = (Core.Name "pathName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathPatternStartNode :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.NodePattern
pathPatternStartNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
        Core.projectionField = (Core.Name "startNode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathPatternWithCondition :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm (Maybe Syntax.ComplexCondition) -> Phantoms.TTerm Syntax.PathPattern
pathPatternWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pathPatternWithEdge :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.EdgePattern -> Phantoms.TTerm Syntax.PathPattern
pathPatternWithEdge original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathPatternWithEndNode :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm Syntax.PathPattern
pathPatternWithEndNode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathPatternWithPathName :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.PathName -> Phantoms.TTerm Syntax.PathPattern
pathPatternWithPathName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "startNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathPatternWithStartNode :: Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.NodePattern -> Phantoms.TTerm Syntax.PathPattern
pathPatternWithStartNode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pathName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "pathName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "startNode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "edge"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "edge")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "endNode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "endNode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathPattern"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathProjAll :: Phantoms.TTerm Syntax.PathProj
pathProjAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

pathProjLimited :: Phantoms.TTerm Syntax.Number -> Phantoms.TTerm Syntax.PathProj
pathProjLimited x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathProj"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "limited"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

pathQuery :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm (Maybe Syntax.RestrictorExt) -> Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm (Maybe Syntax.GroupBy) -> Phantoms.TTerm (Maybe Syntax.OrderBy) -> Phantoms.TTerm Syntax.PathQuery
pathQuery projection restrictorExt pathPattern groupBy orderBy =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm projection)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Phantoms.unTTerm restrictorExt)},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Phantoms.unTTerm pathPattern)},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Phantoms.unTTerm groupBy)},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Phantoms.unTTerm orderBy)}]}))

pathQueryGroupBy :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.GroupBy)
pathQueryGroupBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionField = (Core.Name "groupBy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathQueryOrderBy :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.OrderBy)
pathQueryOrderBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionField = (Core.Name "orderBy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathQueryPathPattern :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm Syntax.PathPattern
pathQueryPathPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionField = (Core.Name "pathPattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathQueryProjection :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm Syntax.Projection
pathQueryProjection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionField = (Core.Name "projection")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathQueryRestrictorExt :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.RestrictorExt)
pathQueryRestrictorExt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
        Core.projectionField = (Core.Name "restrictorExt")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

pathQueryWithGroupBy :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.GroupBy) -> Phantoms.TTerm Syntax.PathQuery
pathQueryWithGroupBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathQueryWithOrderBy :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.OrderBy) -> Phantoms.TTerm Syntax.PathQuery
pathQueryWithOrderBy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pathQueryWithPathPattern :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm Syntax.PathPattern -> Phantoms.TTerm Syntax.PathQuery
pathQueryWithPathPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathQueryWithProjection :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.PathQuery
pathQueryWithProjection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "restrictorExt")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

pathQueryWithRestrictorExt :: Phantoms.TTerm Syntax.PathQuery -> Phantoms.TTerm (Maybe Syntax.RestrictorExt) -> Phantoms.TTerm Syntax.PathQuery
pathQueryWithRestrictorExt original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictorExt"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pathPattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "pathPattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "groupBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "orderBy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.PathQuery"),
              Core.projectionField = (Core.Name "orderBy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

plus :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TTerm Syntax.Plus
plus expression restrictor =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTTerm restrictor)}]}))

plusExpression :: Phantoms.TTerm Syntax.Plus -> Phantoms.TTerm Syntax.Rpq
plusExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

plusRestrictor :: Phantoms.TTerm Syntax.Plus -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor)
plusRestrictor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
        Core.projectionField = (Core.Name "restrictor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

plusWithExpression :: Phantoms.TTerm Syntax.Plus -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Plus
plusWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
              Core.projectionField = (Core.Name "restrictor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

plusWithRestrictor :: Phantoms.TTerm Syntax.Plus -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TTerm Syntax.Plus
plusWithRestrictor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Plus"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projection :: Phantoms.TTerm Syntax.PartProj -> Phantoms.TTerm Syntax.GroupProj -> Phantoms.TTerm Syntax.PathProj -> Phantoms.TTerm Syntax.Projection
projection partProj groupProj pathProj =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Phantoms.unTTerm partProj)},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Phantoms.unTTerm groupProj)},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Phantoms.unTTerm pathProj)}]}))

projectionGroupProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.GroupProj
projectionGroupProj x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionField = (Core.Name "groupProj")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionPartProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.PartProj
projectionPartProj x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionField = (Core.Name "partProj")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionPathProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.PathProj
projectionPathProj x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
        Core.projectionField = (Core.Name "pathProj")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionWithGroupProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.GroupProj -> Phantoms.TTerm Syntax.Projection
projectionWithGroupProj original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "partProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "pathProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionWithPartProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.PartProj -> Phantoms.TTerm Syntax.Projection
projectionWithPartProj original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "groupProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "pathProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionWithPathProj :: Phantoms.TTerm Syntax.Projection -> Phantoms.TTerm Syntax.PathProj -> Phantoms.TTerm Syntax.Projection
projectionWithPathProj original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "partProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "partProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "groupProj"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Projection"),
              Core.projectionField = (Core.Name "groupProj")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pathProj"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

restrictorExtAcyclic :: Phantoms.TTerm Syntax.RestrictorExt
restrictorExtAcyclic =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "acyclic"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtShortest :: Phantoms.TTerm Syntax.RestrictorExt
restrictorExtShortest =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "shortest"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtSimple :: Phantoms.TTerm Syntax.RestrictorExt
restrictorExtSimple =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simple"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtTrail :: Phantoms.TTerm Syntax.RestrictorExt
restrictorExtTrail =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "trail"),
        Core.fieldTerm = Core.TermUnit}}))

restrictorExtWalk :: Phantoms.TTerm Syntax.RestrictorExt
restrictorExtWalk =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RestrictorExt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "walk"),
        Core.fieldTerm = Core.TermUnit}}))

rpqAlternation :: Phantoms.TTerm Syntax.Alternation -> Phantoms.TTerm Syntax.Rpq
rpqAlternation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alternation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqConcatenation :: Phantoms.TTerm Syntax.Concatenation -> Phantoms.TTerm Syntax.Rpq
rpqConcatenation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "concatenation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqLabel :: Phantoms.TTerm Syntax.Label -> Phantoms.TTerm Syntax.Rpq
rpqLabel x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "label"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqNegated :: Phantoms.TTerm Syntax.Label -> Phantoms.TTerm Syntax.Rpq
rpqNegated x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negated"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqOptional :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Rpq
rpqOptional x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "optional"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqParenthesis :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Rpq
rpqParenthesis x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesis"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqPlus :: Phantoms.TTerm Syntax.Plus -> Phantoms.TTerm Syntax.Rpq
rpqPlus x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "plus"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqRestrictor :: Phantoms.TTerm Syntax.RestrictorExt -> Phantoms.TTerm Syntax.RpqRestrictor
rpqRestrictor x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.RpqRestrictor"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

rpqReverse :: Phantoms.TTerm Syntax.Label -> Phantoms.TTerm Syntax.Rpq
rpqReverse x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "reverse"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rpqStar :: Phantoms.TTerm Syntax.Star -> Phantoms.TTerm Syntax.Rpq
rpqStar x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Rpq"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

simpleFunction :: Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.SimpleFunction
simpleFunction name argument =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm argument)}]}))

simpleFunctionArgument :: Phantoms.TTerm Syntax.SimpleFunction -> Phantoms.TTerm Syntax.Text
simpleFunctionArgument x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
        Core.projectionField = (Core.Name "argument")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleFunctionName :: Phantoms.TTerm Syntax.SimpleFunction -> Phantoms.TTerm Syntax.Text
simpleFunctionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simpleFunctionWithArgument :: Phantoms.TTerm Syntax.SimpleFunction -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.SimpleFunction
simpleFunctionWithArgument original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

simpleFunctionWithName :: Phantoms.TTerm Syntax.SimpleFunction -> Phantoms.TTerm Syntax.Text -> Phantoms.TTerm Syntax.SimpleFunction
simpleFunctionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "argument"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.SimpleFunction"),
              Core.projectionField = (Core.Name "argument")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

star :: Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TTerm Syntax.Star
star expression restrictor =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTTerm restrictor)}]}))

starExpression :: Phantoms.TTerm Syntax.Star -> Phantoms.TTerm Syntax.Rpq
starExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

starRestrictor :: Phantoms.TTerm Syntax.Star -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor)
starRestrictor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
        Core.projectionField = (Core.Name "restrictor")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

starWithExpression :: Phantoms.TTerm Syntax.Star -> Phantoms.TTerm Syntax.Rpq -> Phantoms.TTerm Syntax.Star
starWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
              Core.projectionField = (Core.Name "restrictor")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

starWithRestrictor :: Phantoms.TTerm Syntax.Star -> Phantoms.TTerm (Maybe Syntax.RpqRestrictor) -> Phantoms.TTerm Syntax.Star
starWithRestrictor original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "com.gdblab.pathAlgebra.syntax.Star"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "restrictor"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unGroupBy :: Phantoms.TTerm Syntax.GroupBy -> Phantoms.TTerm Syntax.GroupByOption
unGroupBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.GroupBy")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOrderBy :: Phantoms.TTerm Syntax.OrderBy -> Phantoms.TTerm Syntax.OrderByOption
unOrderBy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.OrderBy")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRpqRestrictor :: Phantoms.TTerm Syntax.RpqRestrictor -> Phantoms.TTerm Syntax.RestrictorExt
unRpqRestrictor x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "com.gdblab.pathAlgebra.syntax.RpqRestrictor")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
