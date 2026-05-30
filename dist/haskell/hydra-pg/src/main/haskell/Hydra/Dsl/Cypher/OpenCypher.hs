-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.cypher.openCypher

module Hydra.Dsl.Cypher.OpenCypher where

import qualified Hydra.Core as Core
import qualified Hydra.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

addOrSubtractExpression :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm [OpenCypher.AddOrSubtractRightHandSide] -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

addOrSubtractExpressionLeft :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression
addOrSubtractExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

addOrSubtractExpressionRight :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm [OpenCypher.AddOrSubtractRightHandSide]
addOrSubtractExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

addOrSubtractExpressionWithLeft :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

addOrSubtractExpressionWithRight :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm [OpenCypher.AddOrSubtractRightHandSide] -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

addOrSubtractOperatorAdd :: Phantoms.TypedTerm OpenCypher.AddOrSubtractOperator
addOrSubtractOperatorAdd =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))

addOrSubtractOperatorSubtract :: Phantoms.TypedTerm OpenCypher.AddOrSubtractOperator
addOrSubtractOperatorSubtract =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))

addOrSubtractRightHandSide :: Phantoms.TypedTerm OpenCypher.AddOrSubtractOperator -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSide operator expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

addOrSubtractRightHandSideExpression :: Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression
addOrSubtractRightHandSideExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

addOrSubtractRightHandSideOperator :: Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TypedTerm OpenCypher.AddOrSubtractOperator
addOrSubtractRightHandSideOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

addOrSubtractRightHandSideWithExpression :: Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSideWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

addOrSubtractRightHandSideWithOperator :: Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TypedTerm OpenCypher.AddOrSubtractOperator -> Phantoms.TypedTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSideWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

andExpression :: Phantoms.TypedTerm [OpenCypher.NotExpression] -> Phantoms.TypedTerm OpenCypher.AndExpression
andExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.AndExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

anonymousPatternPart :: Phantoms.TypedTerm OpenCypher.PatternElement -> Phantoms.TypedTerm OpenCypher.AnonymousPatternPart
anonymousPatternPart x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.AnonymousPatternPart"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

atomCase :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm OpenCypher.Atom
atomCase x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomCountStar :: Phantoms.TypedTerm OpenCypher.Atom
atomCountStar =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "countStar"),
        Core.fieldTerm = Core.TermUnit}}))

atomExistentialSubquery :: Phantoms.TypedTerm OpenCypher.ExistentialSubquery -> Phantoms.TypedTerm OpenCypher.Atom
atomExistentialSubquery x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existentialSubquery"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomFunctionInvocation :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm OpenCypher.Atom
atomFunctionInvocation x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionInvocation"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomListComprehension :: Phantoms.TypedTerm OpenCypher.ListComprehension -> Phantoms.TypedTerm OpenCypher.Atom
atomListComprehension x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listComprehension"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomLiteral :: Phantoms.TypedTerm OpenCypher.Literal -> Phantoms.TypedTerm OpenCypher.Atom
atomLiteral x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomParameter :: Phantoms.TypedTerm OpenCypher.Parameter -> Phantoms.TypedTerm OpenCypher.Atom
atomParameter x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameter"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomParenthesized :: Phantoms.TypedTerm OpenCypher.ParenthesizedExpression -> Phantoms.TypedTerm OpenCypher.Atom
atomParenthesized x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomPatternComprehension :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm OpenCypher.Atom
atomPatternComprehension x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patternComprehension"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomPatternPredicate :: Phantoms.TypedTerm OpenCypher.PatternPredicate -> Phantoms.TypedTerm OpenCypher.Atom
atomPatternPredicate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patternPredicate"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomQuantifier :: Phantoms.TypedTerm OpenCypher.Quantifier -> Phantoms.TypedTerm OpenCypher.Atom
atomQuantifier x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quantifier"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

atomVariable :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Atom
atomVariable x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

caseAlternative :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.CaseAlternative
caseAlternative condition result =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTypedTerm result)}]}))

caseAlternativeCondition :: Phantoms.TypedTerm OpenCypher.CaseAlternative -> Phantoms.TypedTerm OpenCypher.Expression
caseAlternativeCondition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
        Core.projectionFieldName = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

caseAlternativeResult :: Phantoms.TypedTerm OpenCypher.CaseAlternative -> Phantoms.TypedTerm OpenCypher.Expression
caseAlternativeResult x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
        Core.projectionFieldName = (Core.Name "result")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

caseAlternativeWithCondition :: Phantoms.TypedTerm OpenCypher.CaseAlternative -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.CaseAlternative
caseAlternativeWithCondition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
              Core.projectionFieldName = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

caseAlternativeWithResult :: Phantoms.TypedTerm OpenCypher.CaseAlternative -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.CaseAlternative
caseAlternativeWithResult original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
              Core.projectionFieldName = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

caseExpression :: Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm [OpenCypher.CaseAlternative] -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.CaseExpression
caseExpression expression alternatives else_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTypedTerm alternatives)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTypedTerm else_)}]}))

caseExpressionAlternatives :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm [OpenCypher.CaseAlternative]
caseExpressionAlternatives x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionFieldName = (Core.Name "alternatives")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

caseExpressionElse :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression)
caseExpressionElse x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionFieldName = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

caseExpressionExpression :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression)
caseExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

caseExpressionWithAlternatives :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm [OpenCypher.CaseAlternative] -> Phantoms.TypedTerm OpenCypher.CaseExpression
caseExpressionWithAlternatives original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

caseExpressionWithElse :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.CaseExpression
caseExpressionWithElse original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "alternatives")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

caseExpressionWithExpression :: Phantoms.TypedTerm OpenCypher.CaseExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.CaseExpression
caseExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "alternatives")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionFieldName = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

comparisonExpression :: Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm [OpenCypher.PartialComparisonExpression] -> Phantoms.TypedTerm OpenCypher.ComparisonExpression
comparisonExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

comparisonExpressionLeft :: Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression
comparisonExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

comparisonExpressionRight :: Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm [OpenCypher.PartialComparisonExpression]
comparisonExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

comparisonExpressionWithLeft :: Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm OpenCypher.ComparisonExpression
comparisonExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

comparisonExpressionWithRight :: Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm [OpenCypher.PartialComparisonExpression] -> Phantoms.TypedTerm OpenCypher.ComparisonExpression
comparisonExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

comparisonOperatorEq :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorEq =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGt :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorGt =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGte :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorGte =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLt :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorLt =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLte :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorLte =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNeq :: Phantoms.TypedTerm OpenCypher.ComparisonOperator
comparisonOperatorNeq =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = Core.TermUnit}}))

create :: Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm OpenCypher.Create
create x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Create"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

delete :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.Delete
delete detach expressions =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Phantoms.unTypedTerm detach)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTypedTerm expressions)}]}))

deleteDetach :: Phantoms.TypedTerm OpenCypher.Delete -> Phantoms.TypedTerm Bool
deleteDetach x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
        Core.projectionFieldName = (Core.Name "detach")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

deleteExpressions :: Phantoms.TypedTerm OpenCypher.Delete -> Phantoms.TypedTerm [OpenCypher.Expression]
deleteExpressions x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
        Core.projectionFieldName = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

deleteWithDetach :: Phantoms.TypedTerm OpenCypher.Delete -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.Delete
deleteWithDetach original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
              Core.projectionFieldName = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

deleteWithExpressions :: Phantoms.TypedTerm OpenCypher.Delete -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.Delete
deleteWithExpressions original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
              Core.projectionFieldName = (Core.Name "detach")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

existentialSubqueryPattern :: Phantoms.TypedTerm OpenCypher.PatternWhere -> Phantoms.TypedTerm OpenCypher.ExistentialSubquery
existentialSubqueryPattern x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ExistentialSubquery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

existentialSubqueryRegular :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm OpenCypher.ExistentialSubquery
existentialSubqueryRegular x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ExistentialSubquery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

explicitProcedureInvocation :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocation name arguments =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm arguments)}]}))

explicitProcedureInvocationArguments :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm [OpenCypher.Expression]
explicitProcedureInvocationArguments x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

explicitProcedureInvocationName :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.QualifiedName
explicitProcedureInvocationName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

explicitProcedureInvocationWithArguments :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocationWithArguments original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

explicitProcedureInvocationWithName :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocationWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

expression :: Phantoms.TypedTerm OpenCypher.OrExpression -> Phantoms.TypedTerm OpenCypher.Expression
expression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Expression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

filterExpression :: Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.FilterExpression
filterExpression idInColl where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Phantoms.unTypedTerm idInColl)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)}]}))

filterExpressionIdInColl :: Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm OpenCypher.IdInColl
filterExpressionIdInColl x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
        Core.projectionFieldName = (Core.Name "idInColl")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

filterExpressionWhere :: Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
filterExpressionWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

filterExpressionWithIdInColl :: Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm OpenCypher.FilterExpression
filterExpressionWithIdInColl original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

filterExpressionWithWhere :: Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.FilterExpression
filterExpressionWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
              Core.projectionFieldName = (Core.Name "idInColl")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

functionInvocation :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.FunctionInvocation
functionInvocation name distinct arguments =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm arguments)}]}))

functionInvocationArguments :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm [OpenCypher.Expression]
functionInvocationArguments x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionFieldName = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

functionInvocationDistinct :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm Bool
functionInvocationDistinct x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionFieldName = (Core.Name "distinct")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

functionInvocationName :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm OpenCypher.QualifiedName
functionInvocationName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

functionInvocationWithArguments :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.FunctionInvocation
functionInvocationWithArguments original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

functionInvocationWithDistinct :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.FunctionInvocation
functionInvocationWithDistinct original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

functionInvocationWithName :: Phantoms.TypedTerm OpenCypher.FunctionInvocation -> Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm OpenCypher.FunctionInvocation
functionInvocationWithName original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionFieldName = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

idInColl :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.IdInColl
idInColl variable expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

idInCollExpression :: Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm OpenCypher.Expression
idInCollExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

idInCollVariable :: Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm OpenCypher.Variable
idInCollVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

idInCollWithExpression :: Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.IdInColl
idInCollWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

idInCollWithVariable :: Phantoms.TypedTerm OpenCypher.IdInColl -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.IdInColl
idInCollWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

implicitProcedureInvocation :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm OpenCypher.ImplicitProcedureInvocation
implicitProcedureInvocation x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ImplicitProcedureInvocation"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

inQueryCall :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm (Maybe OpenCypher.YieldItems) -> Phantoms.TypedTerm OpenCypher.InQueryCall
inQueryCall call yieldItems =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTypedTerm call)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm yieldItems)}]}))

inQueryCallCall :: Phantoms.TypedTerm OpenCypher.InQueryCall -> Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation
inQueryCallCall x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
        Core.projectionFieldName = (Core.Name "call")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

inQueryCallWithCall :: Phantoms.TypedTerm OpenCypher.InQueryCall -> Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.InQueryCall
inQueryCallWithCall original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
              Core.projectionFieldName = (Core.Name "yieldItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

inQueryCallWithYieldItems :: Phantoms.TypedTerm OpenCypher.InQueryCall -> Phantoms.TypedTerm (Maybe OpenCypher.YieldItems) -> Phantoms.TypedTerm OpenCypher.InQueryCall
inQueryCallWithYieldItems original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
              Core.projectionFieldName = (Core.Name "call")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

inQueryCallYieldItems :: Phantoms.TypedTerm OpenCypher.InQueryCall -> Phantoms.TypedTerm (Maybe OpenCypher.YieldItems)
inQueryCallYieldItems x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
        Core.projectionFieldName = (Core.Name "yieldItems")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

keyValuePair :: Phantoms.TypedTerm OpenCypher.PropertyKeyName -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.KeyValuePair
keyValuePair key value =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm value)}]}))

keyValuePairKey :: Phantoms.TypedTerm OpenCypher.KeyValuePair -> Phantoms.TypedTerm OpenCypher.PropertyKeyName
keyValuePairKey x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
        Core.projectionFieldName = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

keyValuePairValue :: Phantoms.TypedTerm OpenCypher.KeyValuePair -> Phantoms.TypedTerm OpenCypher.Expression
keyValuePairValue x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
        Core.projectionFieldName = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

keyValuePairWithKey :: Phantoms.TypedTerm OpenCypher.KeyValuePair -> Phantoms.TypedTerm OpenCypher.PropertyKeyName -> Phantoms.TypedTerm OpenCypher.KeyValuePair
keyValuePairWithKey original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
              Core.projectionFieldName = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

keyValuePairWithValue :: Phantoms.TypedTerm OpenCypher.KeyValuePair -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.KeyValuePair
keyValuePairWithValue original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
              Core.projectionFieldName = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

limit :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Limit
limit x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Limit"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

listComprehension :: Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.ListComprehension
listComprehension left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

listComprehensionLeft :: Phantoms.TypedTerm OpenCypher.ListComprehension -> Phantoms.TypedTerm OpenCypher.FilterExpression
listComprehensionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

listComprehensionRight :: Phantoms.TypedTerm OpenCypher.ListComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Expression)
listComprehensionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

listComprehensionWithLeft :: Phantoms.TypedTerm OpenCypher.ListComprehension -> Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm OpenCypher.ListComprehension
listComprehensionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

listComprehensionWithRight :: Phantoms.TypedTerm OpenCypher.ListComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.ListComprehension
listComprehensionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

listLiteral :: Phantoms.TypedTerm [OpenCypher.Expression] -> Phantoms.TypedTerm OpenCypher.ListLiteral
listLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ListLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

listOperatorExpressionOrPropertyLookupList :: Phantoms.TypedTerm OpenCypher.ListOperatorExpression -> Phantoms.TypedTerm OpenCypher.ListOperatorExpressionOrPropertyLookup
listOperatorExpressionOrPropertyLookupList x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpressionOrPropertyLookup"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

listOperatorExpressionOrPropertyLookupProperty :: Phantoms.TypedTerm OpenCypher.PropertyLookup -> Phantoms.TypedTerm OpenCypher.ListOperatorExpressionOrPropertyLookup
listOperatorExpressionOrPropertyLookupProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpressionOrPropertyLookup"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

listOperatorExpressionRange :: Phantoms.TypedTerm OpenCypher.RangeExpression -> Phantoms.TypedTerm OpenCypher.ListOperatorExpression
listOperatorExpressionRange x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

listOperatorExpressionSingle :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.ListOperatorExpression
listOperatorExpressionSingle x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

listPredicateExpression :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.ListPredicateExpression
listPredicateExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ListPredicateExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

literalBoolean :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.Literal
literalBoolean x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalList :: Phantoms.TypedTerm OpenCypher.ListLiteral -> Phantoms.TypedTerm OpenCypher.Literal
literalList x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalMap :: Phantoms.TypedTerm OpenCypher.MapLiteral -> Phantoms.TypedTerm OpenCypher.Literal
literalMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalNull :: Phantoms.TypedTerm OpenCypher.Literal
literalNull =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

literalNumber :: Phantoms.TypedTerm OpenCypher.NumberLiteral -> Phantoms.TypedTerm OpenCypher.Literal
literalNumber x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

literalString :: Phantoms.TypedTerm OpenCypher.StringLiteral -> Phantoms.TypedTerm OpenCypher.Literal
literalString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

mapLiteral :: Phantoms.TypedTerm [OpenCypher.KeyValuePair] -> Phantoms.TypedTerm OpenCypher.MapLiteral
mapLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.MapLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

match :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.Match
match optional pattern where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
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

matchOptional :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm Bool
matchOptional x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionFieldName = (Core.Name "optional")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchOrCreateCreate :: Phantoms.TypedTerm OpenCypher.MatchOrCreate
matchOrCreateCreate =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MatchOrCreate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "create"),
        Core.fieldTerm = Core.TermUnit}}))

matchOrCreateMatch :: Phantoms.TypedTerm OpenCypher.MatchOrCreate
matchOrCreateMatch =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MatchOrCreate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = Core.TermUnit}}))

matchPattern :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm OpenCypher.Pattern
matchPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchWhere :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
matchWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

matchWithOptional :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.Match
matchWithOptional original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

matchWithPattern :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm OpenCypher.Match
matchWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

matchWithWhere :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.Match
matchWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

merge :: Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm [OpenCypher.MergeAction] -> Phantoms.TypedTerm OpenCypher.Merge
merge patternPart actions =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Phantoms.unTypedTerm patternPart)},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Phantoms.unTypedTerm actions)}]}))

mergeAction :: Phantoms.TypedTerm OpenCypher.MatchOrCreate -> Phantoms.TypedTerm OpenCypher.Set -> Phantoms.TypedTerm OpenCypher.MergeAction
mergeAction action set =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Phantoms.unTypedTerm action)},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Phantoms.unTypedTerm set)}]}))

mergeActionAction :: Phantoms.TypedTerm OpenCypher.MergeAction -> Phantoms.TypedTerm OpenCypher.MatchOrCreate
mergeActionAction x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
        Core.projectionFieldName = (Core.Name "action")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

mergeActionSet :: Phantoms.TypedTerm OpenCypher.MergeAction -> Phantoms.TypedTerm OpenCypher.Set
mergeActionSet x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
        Core.projectionFieldName = (Core.Name "set")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

mergeActionWithAction :: Phantoms.TypedTerm OpenCypher.MergeAction -> Phantoms.TypedTerm OpenCypher.MatchOrCreate -> Phantoms.TypedTerm OpenCypher.MergeAction
mergeActionWithAction original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
              Core.projectionFieldName = (Core.Name "set")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

mergeActionWithSet :: Phantoms.TypedTerm OpenCypher.MergeAction -> Phantoms.TypedTerm OpenCypher.Set -> Phantoms.TypedTerm OpenCypher.MergeAction
mergeActionWithSet original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
              Core.projectionFieldName = (Core.Name "action")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

mergeActions :: Phantoms.TypedTerm OpenCypher.Merge -> Phantoms.TypedTerm [OpenCypher.MergeAction]
mergeActions x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
        Core.projectionFieldName = (Core.Name "actions")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

mergePatternPart :: Phantoms.TypedTerm OpenCypher.Merge -> Phantoms.TypedTerm OpenCypher.PatternPart
mergePatternPart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
        Core.projectionFieldName = (Core.Name "patternPart")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

mergeWithActions :: Phantoms.TypedTerm OpenCypher.Merge -> Phantoms.TypedTerm [OpenCypher.MergeAction] -> Phantoms.TypedTerm OpenCypher.Merge
mergeWithActions original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
              Core.projectionFieldName = (Core.Name "patternPart")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

mergeWithPatternPart :: Phantoms.TypedTerm OpenCypher.Merge -> Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm OpenCypher.Merge
mergeWithPatternPart original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
              Core.projectionFieldName = (Core.Name "actions")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

multiPartQuery :: Phantoms.TypedTerm [OpenCypher.WithClause] -> Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm OpenCypher.MultiPartQuery
multiPartQuery with body =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm body)}]}))

multiPartQueryBody :: Phantoms.TypedTerm OpenCypher.MultiPartQuery -> Phantoms.TypedTerm OpenCypher.SinglePartQuery
multiPartQueryBody x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
        Core.projectionFieldName = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiPartQueryWith :: Phantoms.TypedTerm OpenCypher.MultiPartQuery -> Phantoms.TypedTerm [OpenCypher.WithClause]
multiPartQueryWith x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiPartQueryWithBody :: Phantoms.TypedTerm OpenCypher.MultiPartQuery -> Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm OpenCypher.MultiPartQuery
multiPartQueryWithBody original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

multiPartQueryWithWith :: Phantoms.TypedTerm OpenCypher.MultiPartQuery -> Phantoms.TypedTerm [OpenCypher.WithClause] -> Phantoms.TypedTerm OpenCypher.MultiPartQuery
multiPartQueryWithWith original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
              Core.projectionFieldName = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

multiplyDivideModuloExpression :: Phantoms.TypedTerm OpenCypher.PowerOfExpression -> Phantoms.TypedTerm [OpenCypher.MultiplyDivideModuloRightHandSide] -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

multiplyDivideModuloExpressionLeft :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm OpenCypher.PowerOfExpression
multiplyDivideModuloExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiplyDivideModuloExpressionRight :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm [OpenCypher.MultiplyDivideModuloRightHandSide]
multiplyDivideModuloExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiplyDivideModuloExpressionWithLeft :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm OpenCypher.PowerOfExpression -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

multiplyDivideModuloExpressionWithRight :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TypedTerm [OpenCypher.MultiplyDivideModuloRightHandSide] -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

multiplyDivideModuloOperatorDivide :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorDivide =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloOperatorModulo :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorModulo =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloOperatorMultiply :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorMultiply =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloRightHandSide :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator -> Phantoms.TypedTerm OpenCypher.PowerOfExpression -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSide operator expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

multiplyDivideModuloRightHandSideExpression :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TypedTerm OpenCypher.PowerOfExpression
multiplyDivideModuloRightHandSideExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiplyDivideModuloRightHandSideOperator :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloRightHandSideOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

multiplyDivideModuloRightHandSideWithExpression :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TypedTerm OpenCypher.PowerOfExpression -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSideWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

multiplyDivideModuloRightHandSideWithOperator :: Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloOperator -> Phantoms.TypedTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSideWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodeLabel :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.NodeLabel
nodeLabel x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NodeLabel"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

nodeLabels :: Phantoms.TypedTerm [OpenCypher.NodeLabel] -> Phantoms.TypedTerm OpenCypher.NodeLabels
nodeLabels x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NodeLabels"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

nodePattern :: Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TypedTerm (Maybe OpenCypher.Properties) -> Phantoms.TypedTerm OpenCypher.NodePattern
nodePattern variable labels properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))

nodePatternChain :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm [OpenCypher.PatternElementChain] -> Phantoms.TypedTerm OpenCypher.NodePatternChain
nodePatternChain nodePattern chain =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm nodePattern)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTypedTerm chain)}]}))

nodePatternChainChain :: Phantoms.TypedTerm OpenCypher.NodePatternChain -> Phantoms.TypedTerm [OpenCypher.PatternElementChain]
nodePatternChainChain x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
        Core.projectionFieldName = (Core.Name "chain")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternChainNodePattern :: Phantoms.TypedTerm OpenCypher.NodePatternChain -> Phantoms.TypedTerm OpenCypher.NodePattern
nodePatternChainNodePattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
        Core.projectionFieldName = (Core.Name "nodePattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternChainWithChain :: Phantoms.TypedTerm OpenCypher.NodePatternChain -> Phantoms.TypedTerm [OpenCypher.PatternElementChain] -> Phantoms.TypedTerm OpenCypher.NodePatternChain
nodePatternChainWithChain original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
              Core.projectionFieldName = (Core.Name "nodePattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nodePatternChainWithNodePattern :: Phantoms.TypedTerm OpenCypher.NodePatternChain -> Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm OpenCypher.NodePatternChain
nodePatternChainWithNodePattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
              Core.projectionFieldName = (Core.Name "chain")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodePatternLabels :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels)
nodePatternLabels x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionFieldName = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternProperties :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.Properties)
nodePatternProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternVariable :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.Variable)
nodePatternVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodePatternWithLabels :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TypedTerm OpenCypher.NodePattern
nodePatternWithLabels original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodePatternWithProperties :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.Properties) -> Phantoms.TypedTerm OpenCypher.NodePattern
nodePatternWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nodePatternWithVariable :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.NodePattern
nodePatternWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nonArithmeticOperatorExpression :: Phantoms.TypedTerm OpenCypher.Atom -> Phantoms.TypedTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup] -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpression atom listsAndLookups labels =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTypedTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Phantoms.unTypedTerm listsAndLookups)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm labels)}]}))

nonArithmeticOperatorExpressionAtom :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm OpenCypher.Atom
nonArithmeticOperatorExpressionAtom x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionFieldName = (Core.Name "atom")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nonArithmeticOperatorExpressionLabels :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels)
nonArithmeticOperatorExpressionLabels x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionFieldName = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nonArithmeticOperatorExpressionListsAndLookups :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup]
nonArithmeticOperatorExpressionListsAndLookups x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionFieldName = (Core.Name "listsAndLookups")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nonArithmeticOperatorExpressionWithAtom :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm OpenCypher.Atom -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithAtom original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "listsAndLookups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nonArithmeticOperatorExpressionWithLabels :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithLabels original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "listsAndLookups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nonArithmeticOperatorExpressionWithListsAndLookups :: Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup] -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithListsAndLookups original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

notExpression :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm OpenCypher.NotExpression
notExpression not expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTypedTerm not)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

notExpressionExpression :: Phantoms.TypedTerm OpenCypher.NotExpression -> Phantoms.TypedTerm OpenCypher.ComparisonExpression
notExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

notExpressionNot :: Phantoms.TypedTerm OpenCypher.NotExpression -> Phantoms.TypedTerm Bool
notExpressionNot x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
        Core.projectionFieldName = (Core.Name "not")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

notExpressionWithExpression :: Phantoms.TypedTerm OpenCypher.NotExpression -> Phantoms.TypedTerm OpenCypher.ComparisonExpression -> Phantoms.TypedTerm OpenCypher.NotExpression
notExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
              Core.projectionFieldName = (Core.Name "not")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

notExpressionWithNot :: Phantoms.TypedTerm OpenCypher.NotExpression -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.NotExpression
notExpressionWithNot original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nullPredicateExpression :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.NullPredicateExpression
nullPredicateExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NullPredicateExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

numberLiteralDouble :: Phantoms.TypedTerm Double -> Phantoms.TypedTerm OpenCypher.NumberLiteral
numberLiteralDouble x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.NumberLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

numberLiteralInteger :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm OpenCypher.NumberLiteral
numberLiteralInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.NumberLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

orExpression :: Phantoms.TypedTerm [OpenCypher.XorExpression] -> Phantoms.TypedTerm OpenCypher.OrExpression
orExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.OrExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

order :: Phantoms.TypedTerm [OpenCypher.SortItem] -> Phantoms.TypedTerm OpenCypher.Order
order x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Order"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

parameterInteger :: Phantoms.TypedTerm Integer -> Phantoms.TypedTerm OpenCypher.Parameter
parameterInteger x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Parameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

parameterSymbolic :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.Parameter
parameterSymbolic x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Parameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbolic"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

parenthesizedExpression :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.ParenthesizedExpression
parenthesizedExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ParenthesizedExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

partialComparisonExpression :: Phantoms.TypedTerm OpenCypher.ComparisonOperator -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm OpenCypher.PartialComparisonExpression
partialComparisonExpression operator right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

partialComparisonExpressionOperator :: Phantoms.TypedTerm OpenCypher.PartialComparisonExpression -> Phantoms.TypedTerm OpenCypher.ComparisonOperator
partialComparisonExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

partialComparisonExpressionRight :: Phantoms.TypedTerm OpenCypher.PartialComparisonExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression
partialComparisonExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

partialComparisonExpressionWithOperator :: Phantoms.TypedTerm OpenCypher.PartialComparisonExpression -> Phantoms.TypedTerm OpenCypher.ComparisonOperator -> Phantoms.TypedTerm OpenCypher.PartialComparisonExpression
partialComparisonExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

partialComparisonExpressionWithRight :: Phantoms.TypedTerm OpenCypher.PartialComparisonExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm OpenCypher.PartialComparisonExpression
partialComparisonExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

pattern :: Phantoms.TypedTerm [OpenCypher.PatternPart] -> Phantoms.TypedTerm OpenCypher.Pattern
pattern x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Pattern"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

patternComprehension :: Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.PatternComprehension
patternComprehension variable pattern where_ right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

patternComprehensionPattern :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern
patternComprehensionPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternComprehensionRight :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm OpenCypher.Expression
patternComprehensionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternComprehensionVariable :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Variable)
patternComprehensionVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternComprehensionWhere :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
patternComprehensionWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternComprehensionWithPattern :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm OpenCypher.PatternComprehension
patternComprehensionWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternComprehensionWithRight :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.PatternComprehension
patternComprehensionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

patternComprehensionWithVariable :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.PatternComprehension
patternComprehensionWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternComprehensionWithWhere :: Phantoms.TypedTerm OpenCypher.PatternComprehension -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.PatternComprehension
patternComprehensionWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternElementChain :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm OpenCypher.PatternElementChain
patternElementChain relationship node =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Phantoms.unTypedTerm relationship)},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Phantoms.unTypedTerm node)}]}))

patternElementChainNode :: Phantoms.TypedTerm OpenCypher.PatternElementChain -> Phantoms.TypedTerm OpenCypher.NodePattern
patternElementChainNode x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
        Core.projectionFieldName = (Core.Name "node")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternElementChainRelationship :: Phantoms.TypedTerm OpenCypher.PatternElementChain -> Phantoms.TypedTerm OpenCypher.RelationshipPattern
patternElementChainRelationship x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
        Core.projectionFieldName = (Core.Name "relationship")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternElementChainWithNode :: Phantoms.TypedTerm OpenCypher.PatternElementChain -> Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm OpenCypher.PatternElementChain
patternElementChainWithNode original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
              Core.projectionFieldName = (Core.Name "relationship")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

patternElementChainWithRelationship :: Phantoms.TypedTerm OpenCypher.PatternElementChain -> Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm OpenCypher.PatternElementChain
patternElementChainWithRelationship original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
              Core.projectionFieldName = (Core.Name "node")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternElementChained :: Phantoms.TypedTerm OpenCypher.NodePatternChain -> Phantoms.TypedTerm OpenCypher.PatternElement
patternElementChained x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "chained"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

patternElementParenthesized :: Phantoms.TypedTerm OpenCypher.PatternElement -> Phantoms.TypedTerm OpenCypher.PatternElement
patternElementParenthesized x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

patternPart :: Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.AnonymousPatternPart -> Phantoms.TypedTerm OpenCypher.PatternPart
patternPart variable pattern =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)}]}))

patternPartPattern :: Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm OpenCypher.AnonymousPatternPart
patternPartPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternPartVariable :: Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm (Maybe OpenCypher.Variable)
patternPartVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternPartWithPattern :: Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm OpenCypher.AnonymousPatternPart -> Phantoms.TypedTerm OpenCypher.PatternPart
patternPartWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

patternPartWithVariable :: Phantoms.TypedTerm OpenCypher.PatternPart -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.PatternPart
patternPartWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternPredicate :: Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm OpenCypher.PatternPredicate
patternPredicate x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PatternPredicate"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

patternWhere :: Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.PatternWhere
patternWhere pattern where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)}]}))

patternWherePattern :: Phantoms.TypedTerm OpenCypher.PatternWhere -> Phantoms.TypedTerm OpenCypher.Pattern
patternWherePattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
        Core.projectionFieldName = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternWhereWhere :: Phantoms.TypedTerm OpenCypher.PatternWhere -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
patternWhereWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

patternWhereWithPattern :: Phantoms.TypedTerm OpenCypher.PatternWhere -> Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm OpenCypher.PatternWhere
patternWhereWithPattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

patternWhereWithWhere :: Phantoms.TypedTerm OpenCypher.PatternWhere -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.PatternWhere
patternWhereWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
              Core.projectionFieldName = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

powerOfExpression :: Phantoms.TypedTerm [OpenCypher.UnaryAddOrSubtractExpression] -> Phantoms.TypedTerm OpenCypher.PowerOfExpression
powerOfExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PowerOfExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

procedureInvocationExplicit :: Phantoms.TypedTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.ProcedureInvocation
procedureInvocationExplicit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureInvocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

procedureInvocationImplicit :: Phantoms.TypedTerm OpenCypher.ImplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.ProcedureInvocation
procedureInvocationImplicit x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureInvocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

procedureResultField :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.ProcedureResultField
procedureResultField x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureResultField"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

projectionBody :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm (Maybe OpenCypher.Order) -> Phantoms.TypedTerm (Maybe OpenCypher.Skip) -> Phantoms.TypedTerm (Maybe OpenCypher.Limit) -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBody distinct projectionItems order skip limit =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm projectionItems)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Phantoms.unTypedTerm skip)},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Phantoms.unTypedTerm limit)}]}))

projectionBodyDistinct :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm Bool
projectionBodyDistinct x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionFieldName = (Core.Name "distinct")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionBodyLimit :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Limit)
projectionBodyLimit x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionFieldName = (Core.Name "limit")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionBodyOrder :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Order)
projectionBodyOrder x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionFieldName = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionBodyProjectionItems :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm OpenCypher.ProjectionItems
projectionBodyProjectionItems x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionFieldName = (Core.Name "projectionItems")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionBodySkip :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Skip)
projectionBodySkip x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionFieldName = (Core.Name "skip")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionBodyWithDistinct :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBodyWithDistinct original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionBodyWithLimit :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Limit) -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBodyWithLimit original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projectionBodyWithOrder :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Order) -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBodyWithOrder original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionBodyWithProjectionItems :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBodyWithProjectionItems original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionBodyWithSkip :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Skip) -> Phantoms.TypedTerm OpenCypher.ProjectionBody
projectionBodyWithSkip original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionFieldName = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionItem :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.ProjectionItem
projectionItem expression variable =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)}]}))

projectionItemExpression :: Phantoms.TypedTerm OpenCypher.ProjectionItem -> Phantoms.TypedTerm OpenCypher.Expression
projectionItemExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionItemVariable :: Phantoms.TypedTerm OpenCypher.ProjectionItem -> Phantoms.TypedTerm (Maybe OpenCypher.Variable)
projectionItemVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionItemWithExpression :: Phantoms.TypedTerm OpenCypher.ProjectionItem -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.ProjectionItem
projectionItemWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

projectionItemWithVariable :: Phantoms.TypedTerm OpenCypher.ProjectionItem -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.ProjectionItem
projectionItemWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projectionItems :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm [OpenCypher.ProjectionItem] -> Phantoms.TypedTerm OpenCypher.ProjectionItems
projectionItems star explicit =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Phantoms.unTypedTerm star)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTypedTerm explicit)}]}))

projectionItemsExplicit :: Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm [OpenCypher.ProjectionItem]
projectionItemsExplicit x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
        Core.projectionFieldName = (Core.Name "explicit")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionItemsStar :: Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm Bool
projectionItemsStar x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
        Core.projectionFieldName = (Core.Name "star")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

projectionItemsWithExplicit :: Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm [OpenCypher.ProjectionItem] -> Phantoms.TypedTerm OpenCypher.ProjectionItems
projectionItemsWithExplicit original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
              Core.projectionFieldName = (Core.Name "star")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

projectionItemsWithStar :: Phantoms.TypedTerm OpenCypher.ProjectionItems -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.ProjectionItems
projectionItemsWithStar original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
              Core.projectionFieldName = (Core.Name "explicit")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertiesMap :: Phantoms.TypedTerm OpenCypher.MapLiteral -> Phantoms.TypedTerm OpenCypher.Properties
propertiesMap x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Properties"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertiesParameter :: Phantoms.TypedTerm OpenCypher.Parameter -> Phantoms.TypedTerm OpenCypher.Properties
propertiesParameter x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Properties"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameter"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

propertyEquals :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.PropertyEquals
propertyEquals lhs rhs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm rhs)}]}))

propertyEqualsLhs :: Phantoms.TypedTerm OpenCypher.PropertyEquals -> Phantoms.TypedTerm OpenCypher.PropertyExpression
propertyEqualsLhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyEqualsRhs :: Phantoms.TypedTerm OpenCypher.PropertyEquals -> Phantoms.TypedTerm OpenCypher.Expression
propertyEqualsRhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyEqualsWithLhs :: Phantoms.TypedTerm OpenCypher.PropertyEquals -> Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm OpenCypher.PropertyEquals
propertyEqualsWithLhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyEqualsWithRhs :: Phantoms.TypedTerm OpenCypher.PropertyEquals -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.PropertyEquals
propertyEqualsWithRhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyExpression :: Phantoms.TypedTerm OpenCypher.Atom -> Phantoms.TypedTerm [OpenCypher.PropertyLookup] -> Phantoms.TypedTerm OpenCypher.PropertyExpression
propertyExpression atom lookups =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTypedTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Phantoms.unTypedTerm lookups)}]}))

propertyExpressionAtom :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm OpenCypher.Atom
propertyExpressionAtom x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
        Core.projectionFieldName = (Core.Name "atom")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyExpressionLookups :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm [OpenCypher.PropertyLookup]
propertyExpressionLookups x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
        Core.projectionFieldName = (Core.Name "lookups")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

propertyExpressionWithAtom :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm OpenCypher.Atom -> Phantoms.TypedTerm OpenCypher.PropertyExpression
propertyExpressionWithAtom original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
              Core.projectionFieldName = (Core.Name "lookups")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

propertyExpressionWithLookups :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm [OpenCypher.PropertyLookup] -> Phantoms.TypedTerm OpenCypher.PropertyExpression
propertyExpressionWithLookups original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
              Core.projectionFieldName = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

propertyKeyName :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.PropertyKeyName
propertyKeyName x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PropertyKeyName"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

propertyLookup :: Phantoms.TypedTerm OpenCypher.PropertyKeyName -> Phantoms.TypedTerm OpenCypher.PropertyLookup
propertyLookup x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PropertyLookup"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

qualifiedName :: Phantoms.TypedTerm String -> Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.QualifiedName
qualifiedName namespace local =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTypedTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTypedTerm local)}]}))

qualifiedNameLocal :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm String
qualifiedNameLocal x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

qualifiedNameNamespace :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm String
qualifiedNameNamespace x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
        Core.projectionFieldName = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

qualifiedNameWithLocal :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.QualifiedName
qualifiedNameWithLocal original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
              Core.projectionFieldName = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

qualifiedNameWithNamespace :: Phantoms.TypedTerm OpenCypher.QualifiedName -> Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.QualifiedName
qualifiedNameWithNamespace original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

quantifier :: Phantoms.TypedTerm OpenCypher.QuantifierOperator -> Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm OpenCypher.Quantifier
quantifier operator expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

quantifierExpression :: Phantoms.TypedTerm OpenCypher.Quantifier -> Phantoms.TypedTerm OpenCypher.FilterExpression
quantifierExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

quantifierOperator :: Phantoms.TypedTerm OpenCypher.Quantifier -> Phantoms.TypedTerm OpenCypher.QuantifierOperator
quantifierOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

quantifierOperatorAll :: Phantoms.TypedTerm OpenCypher.QuantifierOperator
quantifierOperatorAll =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorAny :: Phantoms.TypedTerm OpenCypher.QuantifierOperator
quantifierOperatorAny =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorNone :: Phantoms.TypedTerm OpenCypher.QuantifierOperator
quantifierOperatorNone =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorSingle :: Phantoms.TypedTerm OpenCypher.QuantifierOperator
quantifierOperatorSingle =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierWithExpression :: Phantoms.TypedTerm OpenCypher.Quantifier -> Phantoms.TypedTerm OpenCypher.FilterExpression -> Phantoms.TypedTerm OpenCypher.Quantifier
quantifierWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

quantifierWithOperator :: Phantoms.TypedTerm OpenCypher.Quantifier -> Phantoms.TypedTerm OpenCypher.QuantifierOperator -> Phantoms.TypedTerm OpenCypher.Quantifier
quantifierWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

queryRegular :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm OpenCypher.Query
queryRegular x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

queryStandalone :: Phantoms.TypedTerm OpenCypher.StandaloneCall -> Phantoms.TypedTerm OpenCypher.Query
queryStandalone x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "standalone"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

rangeExpression :: Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.RangeExpression
rangeExpression start end =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm end)}]}))

rangeExpressionEnd :: Phantoms.TypedTerm OpenCypher.RangeExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression)
rangeExpressionEnd x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
        Core.projectionFieldName = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeExpressionStart :: Phantoms.TypedTerm OpenCypher.RangeExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression)
rangeExpressionStart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
        Core.projectionFieldName = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeExpressionWithEnd :: Phantoms.TypedTerm OpenCypher.RangeExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.RangeExpression
rangeExpressionWithEnd original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rangeExpressionWithStart :: Phantoms.TypedTerm OpenCypher.RangeExpression -> Phantoms.TypedTerm (Maybe OpenCypher.Expression) -> Phantoms.TypedTerm OpenCypher.RangeExpression
rangeExpressionWithStart original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
              Core.projectionFieldName = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

rangeLiteral :: Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm OpenCypher.RangeLiteral
rangeLiteral start end =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm end)}]}))

rangeLiteralEnd :: Phantoms.TypedTerm OpenCypher.RangeLiteral -> Phantoms.TypedTerm (Maybe Integer)
rangeLiteralEnd x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
        Core.projectionFieldName = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeLiteralStart :: Phantoms.TypedTerm OpenCypher.RangeLiteral -> Phantoms.TypedTerm (Maybe Integer)
rangeLiteralStart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
        Core.projectionFieldName = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

rangeLiteralWithEnd :: Phantoms.TypedTerm OpenCypher.RangeLiteral -> Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm OpenCypher.RangeLiteral
rangeLiteralWithEnd original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
              Core.projectionFieldName = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

rangeLiteralWithStart :: Phantoms.TypedTerm OpenCypher.RangeLiteral -> Phantoms.TypedTerm (Maybe Integer) -> Phantoms.TypedTerm OpenCypher.RangeLiteral
rangeLiteralWithStart original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
              Core.projectionFieldName = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

readingClauseInQueryCall :: Phantoms.TypedTerm OpenCypher.InQueryCall -> Phantoms.TypedTerm OpenCypher.ReadingClause
readingClauseInQueryCall x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inQueryCall"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

readingClauseMatch :: Phantoms.TypedTerm OpenCypher.Match -> Phantoms.TypedTerm OpenCypher.ReadingClause
readingClauseMatch x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

readingClauseUnwind :: Phantoms.TypedTerm OpenCypher.Unwind -> Phantoms.TypedTerm OpenCypher.ReadingClause
readingClauseUnwind x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwind"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

regularQuery :: Phantoms.TypedTerm OpenCypher.SingleQuery -> Phantoms.TypedTerm [OpenCypher.Union] -> Phantoms.TypedTerm OpenCypher.RegularQuery
regularQuery head rest =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTypedTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm rest)}]}))

regularQueryHead :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm OpenCypher.SingleQuery
regularQueryHead x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
        Core.projectionFieldName = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

regularQueryRest :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm [OpenCypher.Union]
regularQueryRest x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
        Core.projectionFieldName = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

regularQueryWithHead :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm OpenCypher.SingleQuery -> Phantoms.TypedTerm OpenCypher.RegularQuery
regularQueryWithHead original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
              Core.projectionFieldName = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

regularQueryWithRest :: Phantoms.TypedTerm OpenCypher.RegularQuery -> Phantoms.TypedTerm [OpenCypher.Union] -> Phantoms.TypedTerm OpenCypher.RegularQuery
regularQueryWithRest original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
              Core.projectionFieldName = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

relTypeName :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.RelTypeName
relTypeName x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.RelTypeName"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

relationshipDetail :: Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipTypes) -> Phantoms.TypedTerm (Maybe OpenCypher.RangeLiteral) -> Phantoms.TypedTerm (Maybe OpenCypher.Properties) -> Phantoms.TypedTerm OpenCypher.RelationshipDetail
relationshipDetail variable types range properties =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTypedTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTypedTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm properties)}]}))

relationshipDetailProperties :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.Properties)
relationshipDetailProperties x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionFieldName = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipDetailRange :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.RangeLiteral)
relationshipDetailRange x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionFieldName = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipDetailTypes :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipTypes)
relationshipDetailTypes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipDetailVariable :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.Variable)
relationshipDetailVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipDetailWithProperties :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.Properties) -> Phantoms.TypedTerm OpenCypher.RelationshipDetail
relationshipDetailWithProperties original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

relationshipDetailWithRange :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.RangeLiteral) -> Phantoms.TypedTerm OpenCypher.RelationshipDetail
relationshipDetailWithRange original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

relationshipDetailWithTypes :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipTypes) -> Phantoms.TypedTerm OpenCypher.RelationshipDetail
relationshipDetailWithTypes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

relationshipDetailWithVariable :: Phantoms.TypedTerm OpenCypher.RelationshipDetail -> Phantoms.TypedTerm (Maybe OpenCypher.Variable) -> Phantoms.TypedTerm OpenCypher.RelationshipDetail
relationshipDetailWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionFieldName = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

relationshipPattern :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipDetail) -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.RelationshipPattern
relationshipPattern leftArrow detail rightArrow =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Phantoms.unTypedTerm leftArrow)},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Phantoms.unTypedTerm detail)},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Phantoms.unTypedTerm rightArrow)}]}))

relationshipPatternDetail :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipDetail)
relationshipPatternDetail x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionFieldName = (Core.Name "detail")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipPatternLeftArrow :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm Bool
relationshipPatternLeftArrow x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionFieldName = (Core.Name "leftArrow")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipPatternRightArrow :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm Bool
relationshipPatternRightArrow x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionFieldName = (Core.Name "rightArrow")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipPatternWithDetail :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm (Maybe OpenCypher.RelationshipDetail) -> Phantoms.TypedTerm OpenCypher.RelationshipPattern
relationshipPatternWithDetail original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "leftArrow")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "rightArrow")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

relationshipPatternWithLeftArrow :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.RelationshipPattern
relationshipPatternWithLeftArrow original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "detail")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "rightArrow")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

relationshipPatternWithRightArrow :: Phantoms.TypedTerm OpenCypher.RelationshipPattern -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.RelationshipPattern
relationshipPatternWithRightArrow original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "leftArrow")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionFieldName = (Core.Name "detail")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

relationshipTypes :: Phantoms.TypedTerm [OpenCypher.RelTypeName] -> Phantoms.TypedTerm OpenCypher.RelationshipTypes
relationshipTypes x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipTypes"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

relationshipsPattern :: Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm [OpenCypher.PatternElementChain] -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern
relationshipsPattern nodePattern chain =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm nodePattern)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTypedTerm chain)}]}))

relationshipsPatternChain :: Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm [OpenCypher.PatternElementChain]
relationshipsPatternChain x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
        Core.projectionFieldName = (Core.Name "chain")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipsPatternNodePattern :: Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm OpenCypher.NodePattern
relationshipsPatternNodePattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
        Core.projectionFieldName = (Core.Name "nodePattern")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

relationshipsPatternWithChain :: Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm [OpenCypher.PatternElementChain] -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern
relationshipsPatternWithChain original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
              Core.projectionFieldName = (Core.Name "nodePattern")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

relationshipsPatternWithNodePattern :: Phantoms.TypedTerm OpenCypher.RelationshipsPattern -> Phantoms.TypedTerm OpenCypher.NodePattern -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern
relationshipsPatternWithNodePattern original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
              Core.projectionFieldName = (Core.Name "chain")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

remove :: Phantoms.TypedTerm [OpenCypher.RemoveItem] -> Phantoms.TypedTerm OpenCypher.Remove
remove x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Remove"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

removeItemProperty :: Phantoms.TypedTerm OpenCypher.PropertyExpression -> Phantoms.TypedTerm OpenCypher.RemoveItem
removeItemProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.RemoveItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

removeItemVariableLabels :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.RemoveItem
removeItemVariableLabels x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.RemoveItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableLabels"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

return :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm OpenCypher.Return
return x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Return"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

set :: Phantoms.TypedTerm [OpenCypher.SetItem] -> Phantoms.TypedTerm OpenCypher.Set
set x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Set"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

setItemProperty :: Phantoms.TypedTerm OpenCypher.PropertyEquals -> Phantoms.TypedTerm OpenCypher.SetItem
setItemProperty x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

setItemVariableEqual :: Phantoms.TypedTerm OpenCypher.VariableEquals -> Phantoms.TypedTerm OpenCypher.SetItem
setItemVariableEqual x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableEqual"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

setItemVariableLabels :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.SetItem
setItemVariableLabels x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableLabels"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

setItemVariablePlusEqual :: Phantoms.TypedTerm OpenCypher.VariablePlusEquals -> Phantoms.TypedTerm OpenCypher.SetItem
setItemVariablePlusEqual x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variablePlusEqual"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

singlePartQuery :: Phantoms.TypedTerm [OpenCypher.ReadingClause] -> Phantoms.TypedTerm [OpenCypher.UpdatingClause] -> Phantoms.TypedTerm (Maybe OpenCypher.Return) -> Phantoms.TypedTerm OpenCypher.SinglePartQuery
singlePartQuery reading updating return =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTypedTerm reading)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTypedTerm updating)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm return)}]}))

singlePartQueryReading :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm [OpenCypher.ReadingClause]
singlePartQueryReading x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionFieldName = (Core.Name "reading")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

singlePartQueryReturn :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm (Maybe OpenCypher.Return)
singlePartQueryReturn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionFieldName = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

singlePartQueryUpdating :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm [OpenCypher.UpdatingClause]
singlePartQueryUpdating x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionFieldName = (Core.Name "updating")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

singlePartQueryWithReading :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm [OpenCypher.ReadingClause] -> Phantoms.TypedTerm OpenCypher.SinglePartQuery
singlePartQueryWithReading original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

singlePartQueryWithReturn :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm (Maybe OpenCypher.Return) -> Phantoms.TypedTerm OpenCypher.SinglePartQuery
singlePartQueryWithReturn original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

singlePartQueryWithUpdating :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm [OpenCypher.UpdatingClause] -> Phantoms.TypedTerm OpenCypher.SinglePartQuery
singlePartQueryWithUpdating original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionFieldName = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

singleQueryMultiPart :: Phantoms.TypedTerm OpenCypher.MultiPartQuery -> Phantoms.TypedTerm OpenCypher.SingleQuery
singleQueryMultiPart x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SingleQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPart"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

singleQuerySinglePart :: Phantoms.TypedTerm OpenCypher.SinglePartQuery -> Phantoms.TypedTerm OpenCypher.SingleQuery
singleQuerySinglePart x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SingleQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singlePart"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

skip :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Skip
skip x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Skip"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

sortItem :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm (Maybe OpenCypher.SortOrder) -> Phantoms.TypedTerm OpenCypher.SortItem
sortItem expression order =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm order)}]}))

sortItemExpression :: Phantoms.TypedTerm OpenCypher.SortItem -> Phantoms.TypedTerm OpenCypher.Expression
sortItemExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

sortItemOrder :: Phantoms.TypedTerm OpenCypher.SortItem -> Phantoms.TypedTerm (Maybe OpenCypher.SortOrder)
sortItemOrder x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
        Core.projectionFieldName = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

sortItemWithExpression :: Phantoms.TypedTerm OpenCypher.SortItem -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.SortItem
sortItemWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
              Core.projectionFieldName = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

sortItemWithOrder :: Phantoms.TypedTerm OpenCypher.SortItem -> Phantoms.TypedTerm (Maybe OpenCypher.SortOrder) -> Phantoms.TypedTerm OpenCypher.SortItem
sortItemWithOrder original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

sortOrderAscending :: Phantoms.TypedTerm OpenCypher.SortOrder
sortOrderAscending =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SortOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascending"),
        Core.fieldTerm = Core.TermUnit}}))

sortOrderDescending :: Phantoms.TypedTerm OpenCypher.SortOrder
sortOrderDescending =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SortOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "descending"),
        Core.fieldTerm = Core.TermUnit}}))

standaloneCall :: Phantoms.TypedTerm OpenCypher.ProcedureInvocation -> Phantoms.TypedTerm (Maybe OpenCypher.StarOrYieldItems) -> Phantoms.TypedTerm OpenCypher.StandaloneCall
standaloneCall call yieldItems =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTypedTerm call)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm yieldItems)}]}))

standaloneCallCall :: Phantoms.TypedTerm OpenCypher.StandaloneCall -> Phantoms.TypedTerm OpenCypher.ProcedureInvocation
standaloneCallCall x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
        Core.projectionFieldName = (Core.Name "call")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

standaloneCallWithCall :: Phantoms.TypedTerm OpenCypher.StandaloneCall -> Phantoms.TypedTerm OpenCypher.ProcedureInvocation -> Phantoms.TypedTerm OpenCypher.StandaloneCall
standaloneCallWithCall original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
              Core.projectionFieldName = (Core.Name "yieldItems")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

standaloneCallWithYieldItems :: Phantoms.TypedTerm OpenCypher.StandaloneCall -> Phantoms.TypedTerm (Maybe OpenCypher.StarOrYieldItems) -> Phantoms.TypedTerm OpenCypher.StandaloneCall
standaloneCallWithYieldItems original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
              Core.projectionFieldName = (Core.Name "call")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

standaloneCallYieldItems :: Phantoms.TypedTerm OpenCypher.StandaloneCall -> Phantoms.TypedTerm (Maybe OpenCypher.StarOrYieldItems)
standaloneCallYieldItems x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
        Core.projectionFieldName = (Core.Name "yieldItems")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

starOrYieldItemsItems :: Phantoms.TypedTerm OpenCypher.YieldItems -> Phantoms.TypedTerm OpenCypher.StarOrYieldItems
starOrYieldItemsItems x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StarOrYieldItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "items"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

starOrYieldItemsStar :: Phantoms.TypedTerm OpenCypher.StarOrYieldItems
starOrYieldItemsStar =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StarOrYieldItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = Core.TermUnit}}))

stringListNullPredicateExpression :: Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm [OpenCypher.StringListNullPredicateRightHandSide] -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpression left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

stringListNullPredicateExpressionLeft :: Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
stringListNullPredicateExpressionLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringListNullPredicateExpressionRight :: Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm [OpenCypher.StringListNullPredicateRightHandSide]
stringListNullPredicateExpressionRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringListNullPredicateExpressionWithLeft :: Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpressionWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringListNullPredicateExpressionWithRight :: Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TypedTerm [OpenCypher.StringListNullPredicateRightHandSide] -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpressionWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringListNullPredicateRightHandSideList :: Phantoms.TypedTerm OpenCypher.ListPredicateExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideList x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringListNullPredicateRightHandSideNull :: Phantoms.TypedTerm OpenCypher.NullPredicateExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideNull x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringListNullPredicateRightHandSideString :: Phantoms.TypedTerm OpenCypher.StringPredicateExpression -> Phantoms.TypedTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideString x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stringLiteral :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.StringLiteral
stringLiteral x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.StringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

stringPredicateExpression :: Phantoms.TypedTerm OpenCypher.StringPredicateOperator -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.StringPredicateExpression
stringPredicateExpression operator expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

stringPredicateExpressionExpression :: Phantoms.TypedTerm OpenCypher.StringPredicateExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
stringPredicateExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringPredicateExpressionOperator :: Phantoms.TypedTerm OpenCypher.StringPredicateExpression -> Phantoms.TypedTerm OpenCypher.StringPredicateOperator
stringPredicateExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

stringPredicateExpressionWithExpression :: Phantoms.TypedTerm OpenCypher.StringPredicateExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.StringPredicateExpression
stringPredicateExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stringPredicateExpressionWithOperator :: Phantoms.TypedTerm OpenCypher.StringPredicateExpression -> Phantoms.TypedTerm OpenCypher.StringPredicateOperator -> Phantoms.TypedTerm OpenCypher.StringPredicateExpression
stringPredicateExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

stringPredicateOperatorContains :: Phantoms.TypedTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorContains =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contains"),
        Core.fieldTerm = Core.TermUnit}}))

stringPredicateOperatorEndsWith :: Phantoms.TypedTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorEndsWith =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endsWith"),
        Core.fieldTerm = Core.TermUnit}}))

stringPredicateOperatorStartsWith :: Phantoms.TypedTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorStartsWith =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startsWith"),
        Core.fieldTerm = Core.TermUnit}}))

unAndExpression :: Phantoms.TypedTerm OpenCypher.AndExpression -> Phantoms.TypedTerm [OpenCypher.NotExpression]
unAndExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.AndExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unAnonymousPatternPart :: Phantoms.TypedTerm OpenCypher.AnonymousPatternPart -> Phantoms.TypedTerm OpenCypher.PatternElement
unAnonymousPatternPart x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.AnonymousPatternPart")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unCreate :: Phantoms.TypedTerm OpenCypher.Create -> Phantoms.TypedTerm OpenCypher.Pattern
unCreate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Create")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unExpression :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.OrExpression
unExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Expression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unImplicitProcedureInvocation :: Phantoms.TypedTerm OpenCypher.ImplicitProcedureInvocation -> Phantoms.TypedTerm OpenCypher.QualifiedName
unImplicitProcedureInvocation x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ImplicitProcedureInvocation")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unLimit :: Phantoms.TypedTerm OpenCypher.Limit -> Phantoms.TypedTerm OpenCypher.Expression
unLimit x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Limit")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unListLiteral :: Phantoms.TypedTerm OpenCypher.ListLiteral -> Phantoms.TypedTerm [OpenCypher.Expression]
unListLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ListLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unListPredicateExpression :: Phantoms.TypedTerm OpenCypher.ListPredicateExpression -> Phantoms.TypedTerm OpenCypher.AddOrSubtractExpression
unListPredicateExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ListPredicateExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unMapLiteral :: Phantoms.TypedTerm OpenCypher.MapLiteral -> Phantoms.TypedTerm [OpenCypher.KeyValuePair]
unMapLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.MapLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unNodeLabel :: Phantoms.TypedTerm OpenCypher.NodeLabel -> Phantoms.TypedTerm String
unNodeLabel x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NodeLabel")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unNodeLabels :: Phantoms.TypedTerm OpenCypher.NodeLabels -> Phantoms.TypedTerm [OpenCypher.NodeLabel]
unNodeLabels x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NodeLabels")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unNullPredicateExpression :: Phantoms.TypedTerm OpenCypher.NullPredicateExpression -> Phantoms.TypedTerm Bool
unNullPredicateExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NullPredicateExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unOrExpression :: Phantoms.TypedTerm OpenCypher.OrExpression -> Phantoms.TypedTerm [OpenCypher.XorExpression]
unOrExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.OrExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unOrder :: Phantoms.TypedTerm OpenCypher.Order -> Phantoms.TypedTerm [OpenCypher.SortItem]
unOrder x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Order")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unParenthesizedExpression :: Phantoms.TypedTerm OpenCypher.ParenthesizedExpression -> Phantoms.TypedTerm OpenCypher.Expression
unParenthesizedExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ParenthesizedExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPattern :: Phantoms.TypedTerm OpenCypher.Pattern -> Phantoms.TypedTerm [OpenCypher.PatternPart]
unPattern x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Pattern")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPatternPredicate :: Phantoms.TypedTerm OpenCypher.PatternPredicate -> Phantoms.TypedTerm OpenCypher.RelationshipsPattern
unPatternPredicate x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PatternPredicate")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPowerOfExpression :: Phantoms.TypedTerm OpenCypher.PowerOfExpression -> Phantoms.TypedTerm [OpenCypher.UnaryAddOrSubtractExpression]
unPowerOfExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PowerOfExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unProcedureResultField :: Phantoms.TypedTerm OpenCypher.ProcedureResultField -> Phantoms.TypedTerm String
unProcedureResultField x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ProcedureResultField")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPropertyKeyName :: Phantoms.TypedTerm OpenCypher.PropertyKeyName -> Phantoms.TypedTerm String
unPropertyKeyName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PropertyKeyName")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unPropertyLookup :: Phantoms.TypedTerm OpenCypher.PropertyLookup -> Phantoms.TypedTerm OpenCypher.PropertyKeyName
unPropertyLookup x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PropertyLookup")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unRelTypeName :: Phantoms.TypedTerm OpenCypher.RelTypeName -> Phantoms.TypedTerm String
unRelTypeName x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.RelTypeName")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unRelationshipTypes :: Phantoms.TypedTerm OpenCypher.RelationshipTypes -> Phantoms.TypedTerm [OpenCypher.RelTypeName]
unRelationshipTypes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.RelationshipTypes")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unRemove :: Phantoms.TypedTerm OpenCypher.Remove -> Phantoms.TypedTerm [OpenCypher.RemoveItem]
unRemove x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Remove")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unReturn :: Phantoms.TypedTerm OpenCypher.Return -> Phantoms.TypedTerm OpenCypher.ProjectionBody
unReturn x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Return")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unSet :: Phantoms.TypedTerm OpenCypher.Set -> Phantoms.TypedTerm [OpenCypher.SetItem]
unSet x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Set")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unSkip :: Phantoms.TypedTerm OpenCypher.Skip -> Phantoms.TypedTerm OpenCypher.Expression
unSkip x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Skip")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unStringLiteral :: Phantoms.TypedTerm OpenCypher.StringLiteral -> Phantoms.TypedTerm String
unStringLiteral x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.StringLiteral")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unVariable :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm String
unVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Variable")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unWhere :: Phantoms.TypedTerm OpenCypher.Where -> Phantoms.TypedTerm OpenCypher.Expression
unWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Where")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unXorExpression :: Phantoms.TypedTerm OpenCypher.XorExpression -> Phantoms.TypedTerm [OpenCypher.AndExpression]
unXorExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.XorExpression")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryAddOrSubtractExpression :: Phantoms.TypedTerm (Maybe OpenCypher.AddOrSubtractOperator) -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpression operator expression =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)}]}))

unaryAddOrSubtractExpressionExpression :: Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression
unaryAddOrSubtractExpressionExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryAddOrSubtractExpressionOperator :: Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TypedTerm (Maybe OpenCypher.AddOrSubtractOperator)
unaryAddOrSubtractExpressionOperator x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
        Core.projectionFieldName = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unaryAddOrSubtractExpressionWithExpression :: Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TypedTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpressionWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
              Core.projectionFieldName = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unaryAddOrSubtractExpressionWithOperator :: Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TypedTerm (Maybe OpenCypher.AddOrSubtractOperator) -> Phantoms.TypedTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpressionWithOperator original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

union :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.SingleQuery -> Phantoms.TypedTerm OpenCypher.Union
union all query =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTypedTerm all)},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Phantoms.unTypedTerm query)}]}))

unionAll :: Phantoms.TypedTerm OpenCypher.Union -> Phantoms.TypedTerm Bool
unionAll x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
        Core.projectionFieldName = (Core.Name "all")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unionQuery :: Phantoms.TypedTerm OpenCypher.Union -> Phantoms.TypedTerm OpenCypher.SingleQuery
unionQuery x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
        Core.projectionFieldName = (Core.Name "query")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unionWithAll :: Phantoms.TypedTerm OpenCypher.Union -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm OpenCypher.Union
unionWithAll original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
              Core.projectionFieldName = (Core.Name "query")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

unionWithQuery :: Phantoms.TypedTerm OpenCypher.Union -> Phantoms.TypedTerm OpenCypher.SingleQuery -> Phantoms.TypedTerm OpenCypher.Union
unionWithQuery original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
              Core.projectionFieldName = (Core.Name "all")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

unwind :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Unwind
unwind expression variable =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)}]}))

unwindExpression :: Phantoms.TypedTerm OpenCypher.Unwind -> Phantoms.TypedTerm OpenCypher.Expression
unwindExpression x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
        Core.projectionFieldName = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unwindVariable :: Phantoms.TypedTerm OpenCypher.Unwind -> Phantoms.TypedTerm OpenCypher.Variable
unwindVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unwindWithExpression :: Phantoms.TypedTerm OpenCypher.Unwind -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Unwind
unwindWithExpression original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

unwindWithVariable :: Phantoms.TypedTerm OpenCypher.Unwind -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Unwind
unwindWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
              Core.projectionFieldName = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

updatingClauseCreate :: Phantoms.TypedTerm OpenCypher.Create -> Phantoms.TypedTerm OpenCypher.UpdatingClause
updatingClauseCreate x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "create"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

updatingClauseDelete :: Phantoms.TypedTerm OpenCypher.Delete -> Phantoms.TypedTerm OpenCypher.UpdatingClause
updatingClauseDelete x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delete"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

updatingClauseMerge :: Phantoms.TypedTerm OpenCypher.Merge -> Phantoms.TypedTerm OpenCypher.UpdatingClause
updatingClauseMerge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "merge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

updatingClauseRemove :: Phantoms.TypedTerm OpenCypher.Remove -> Phantoms.TypedTerm OpenCypher.UpdatingClause
updatingClauseRemove x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remove"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

updatingClauseSet :: Phantoms.TypedTerm OpenCypher.Set -> Phantoms.TypedTerm OpenCypher.UpdatingClause
updatingClauseSet x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

variable :: Phantoms.TypedTerm String -> Phantoms.TypedTerm OpenCypher.Variable
variable x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Variable"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

variableAndNodeLabels :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.NodeLabels -> Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabels variable labels =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm labels)}]}))

variableAndNodeLabelsLabels :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.NodeLabels
variableAndNodeLabelsLabels x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
        Core.projectionFieldName = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableAndNodeLabelsVariable :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.Variable
variableAndNodeLabelsVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableAndNodeLabelsWithLabels :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.NodeLabels -> Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabelsWithLabels original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

variableAndNodeLabelsWithVariable :: Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabelsWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
              Core.projectionFieldName = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

variableEquals :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.VariableEquals
variableEquals lhs rhs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm rhs)}]}))

variableEqualsLhs :: Phantoms.TypedTerm OpenCypher.VariableEquals -> Phantoms.TypedTerm OpenCypher.Variable
variableEqualsLhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableEqualsRhs :: Phantoms.TypedTerm OpenCypher.VariableEquals -> Phantoms.TypedTerm OpenCypher.Expression
variableEqualsRhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variableEqualsWithLhs :: Phantoms.TypedTerm OpenCypher.VariableEquals -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.VariableEquals
variableEqualsWithLhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

variableEqualsWithRhs :: Phantoms.TypedTerm OpenCypher.VariableEquals -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.VariableEquals
variableEqualsWithRhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

variablePlusEquals :: Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.VariablePlusEquals
variablePlusEquals lhs rhs =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm rhs)}]}))

variablePlusEqualsLhs :: Phantoms.TypedTerm OpenCypher.VariablePlusEquals -> Phantoms.TypedTerm OpenCypher.Variable
variablePlusEqualsLhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
        Core.projectionFieldName = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variablePlusEqualsRhs :: Phantoms.TypedTerm OpenCypher.VariablePlusEquals -> Phantoms.TypedTerm OpenCypher.Expression
variablePlusEqualsRhs x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
        Core.projectionFieldName = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

variablePlusEqualsWithLhs :: Phantoms.TypedTerm OpenCypher.VariablePlusEquals -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.VariablePlusEquals
variablePlusEqualsWithLhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
              Core.projectionFieldName = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

variablePlusEqualsWithRhs :: Phantoms.TypedTerm OpenCypher.VariablePlusEquals -> Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.VariablePlusEquals
variablePlusEqualsWithRhs original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
              Core.projectionFieldName = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

where_ :: Phantoms.TypedTerm OpenCypher.Expression -> Phantoms.TypedTerm OpenCypher.Where
where_ x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Where"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

with :: Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.With
with projection where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm projection)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)}]}))

withClause :: Phantoms.TypedTerm [OpenCypher.ReadingClause] -> Phantoms.TypedTerm [OpenCypher.UpdatingClause] -> Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm OpenCypher.WithClause
withClause reading updating with =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTypedTerm reading)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTypedTerm updating)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm with)}]}))

withClauseReading :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm [OpenCypher.ReadingClause]
withClauseReading x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionFieldName = (Core.Name "reading")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withClauseUpdating :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm [OpenCypher.UpdatingClause]
withClauseUpdating x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionFieldName = (Core.Name "updating")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withClauseWith :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm OpenCypher.With
withClauseWith x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionFieldName = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withClauseWithReading :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm [OpenCypher.ReadingClause] -> Phantoms.TypedTerm OpenCypher.WithClause
withClauseWithReading original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

withClauseWithUpdating :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm [OpenCypher.UpdatingClause] -> Phantoms.TypedTerm OpenCypher.WithClause
withClauseWithUpdating original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

withClauseWithWith :: Phantoms.TypedTerm OpenCypher.WithClause -> Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm OpenCypher.WithClause
withClauseWithWith original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionFieldName = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

withProjection :: Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm OpenCypher.ProjectionBody
withProjection x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
        Core.projectionFieldName = (Core.Name "projection")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withWhere :: Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
withWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

withWithProjection :: Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm OpenCypher.ProjectionBody -> Phantoms.TypedTerm OpenCypher.With
withWithProjection original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

withWithWhere :: Phantoms.TypedTerm OpenCypher.With -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.With
withWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
              Core.projectionFieldName = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

xorExpression :: Phantoms.TypedTerm [OpenCypher.AndExpression] -> Phantoms.TypedTerm OpenCypher.XorExpression
xorExpression x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.XorExpression"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

yieldItem :: Phantoms.TypedTerm (Maybe OpenCypher.ProcedureResultField) -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.YieldItem
yieldItem field variable =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTypedTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm variable)}]}))

yieldItemField :: Phantoms.TypedTerm OpenCypher.YieldItem -> Phantoms.TypedTerm (Maybe OpenCypher.ProcedureResultField)
yieldItemField x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

yieldItemVariable :: Phantoms.TypedTerm OpenCypher.YieldItem -> Phantoms.TypedTerm OpenCypher.Variable
yieldItemVariable x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
        Core.projectionFieldName = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

yieldItemWithField :: Phantoms.TypedTerm OpenCypher.YieldItem -> Phantoms.TypedTerm (Maybe OpenCypher.ProcedureResultField) -> Phantoms.TypedTerm OpenCypher.YieldItem
yieldItemWithField original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
              Core.projectionFieldName = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

yieldItemWithVariable :: Phantoms.TypedTerm OpenCypher.YieldItem -> Phantoms.TypedTerm OpenCypher.Variable -> Phantoms.TypedTerm OpenCypher.YieldItem
yieldItemWithVariable original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

yieldItems :: Phantoms.TypedTerm [OpenCypher.YieldItem] -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.YieldItems
yieldItems items where_ =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTypedTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm where_)}]}))

yieldItemsItems :: Phantoms.TypedTerm OpenCypher.YieldItems -> Phantoms.TypedTerm [OpenCypher.YieldItem]
yieldItemsItems x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
        Core.projectionFieldName = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

yieldItemsWhere :: Phantoms.TypedTerm OpenCypher.YieldItems -> Phantoms.TypedTerm (Maybe OpenCypher.Where)
yieldItemsWhere x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
        Core.projectionFieldName = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

yieldItemsWithItems :: Phantoms.TypedTerm OpenCypher.YieldItems -> Phantoms.TypedTerm [OpenCypher.YieldItem] -> Phantoms.TypedTerm OpenCypher.YieldItems
yieldItemsWithItems original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
              Core.projectionFieldName = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

yieldItemsWithWhere :: Phantoms.TypedTerm OpenCypher.YieldItems -> Phantoms.TypedTerm (Maybe OpenCypher.Where) -> Phantoms.TypedTerm OpenCypher.YieldItems
yieldItemsWithWhere original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
              Core.projectionFieldName = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))
