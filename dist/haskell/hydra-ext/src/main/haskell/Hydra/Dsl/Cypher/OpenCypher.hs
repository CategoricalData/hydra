-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.cypher.openCypher

module Hydra.Dsl.Cypher.OpenCypher where

import qualified Hydra.Core as Core
import qualified Hydra.Cypher.OpenCypher as OpenCypher
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

addOrSubtractExpression :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm [OpenCypher.AddOrSubtractRightHandSide] -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

addOrSubtractExpressionLeft :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression
addOrSubtractExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOrSubtractExpressionRight :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm [OpenCypher.AddOrSubtractRightHandSide]
addOrSubtractExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOrSubtractExpressionWithLeft :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

addOrSubtractExpressionWithRight :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm [OpenCypher.AddOrSubtractRightHandSide] -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
addOrSubtractExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

addOrSubtractOperatorAdd :: Phantoms.TTerm OpenCypher.AddOrSubtractOperator
addOrSubtractOperatorAdd =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "add"),
        Core.fieldTerm = Core.TermUnit}}))

addOrSubtractOperatorSubtract :: Phantoms.TTerm OpenCypher.AddOrSubtractOperator
addOrSubtractOperatorSubtract =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subtract"),
        Core.fieldTerm = Core.TermUnit}}))

addOrSubtractRightHandSide :: Phantoms.TTerm OpenCypher.AddOrSubtractOperator -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSide operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

addOrSubtractRightHandSideExpression :: Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression
addOrSubtractRightHandSideExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOrSubtractRightHandSideOperator :: Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TTerm OpenCypher.AddOrSubtractOperator
addOrSubtractRightHandSideOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

addOrSubtractRightHandSideWithExpression :: Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSideWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

addOrSubtractRightHandSideWithOperator :: Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide -> Phantoms.TTerm OpenCypher.AddOrSubtractOperator -> Phantoms.TTerm OpenCypher.AddOrSubtractRightHandSide
addOrSubtractRightHandSideWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.AddOrSubtractRightHandSide"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

andExpression :: Phantoms.TTerm [OpenCypher.NotExpression] -> Phantoms.TTerm OpenCypher.AndExpression
andExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.AndExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

anonymousPatternPart :: Phantoms.TTerm OpenCypher.PatternElement -> Phantoms.TTerm OpenCypher.AnonymousPatternPart
anonymousPatternPart x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.AnonymousPatternPart"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

atomCase :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm OpenCypher.Atom
atomCase x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "case"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomCountStar :: Phantoms.TTerm OpenCypher.Atom
atomCountStar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "countStar"),
        Core.fieldTerm = Core.TermUnit}}))

atomExistentialSubquery :: Phantoms.TTerm OpenCypher.ExistentialSubquery -> Phantoms.TTerm OpenCypher.Atom
atomExistentialSubquery x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "existentialSubquery"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomFunctionInvocation :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm OpenCypher.Atom
atomFunctionInvocation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "functionInvocation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomListComprehension :: Phantoms.TTerm OpenCypher.ListComprehension -> Phantoms.TTerm OpenCypher.Atom
atomListComprehension x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "listComprehension"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomLiteral :: Phantoms.TTerm OpenCypher.Literal -> Phantoms.TTerm OpenCypher.Atom
atomLiteral x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "literal"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomParameter :: Phantoms.TTerm OpenCypher.Parameter -> Phantoms.TTerm OpenCypher.Atom
atomParameter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameter"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomParenthesized :: Phantoms.TTerm OpenCypher.ParenthesizedExpression -> Phantoms.TTerm OpenCypher.Atom
atomParenthesized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomPatternComprehension :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm OpenCypher.Atom
atomPatternComprehension x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patternComprehension"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomPatternPredicate :: Phantoms.TTerm OpenCypher.PatternPredicate -> Phantoms.TTerm OpenCypher.Atom
atomPatternPredicate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "patternPredicate"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomQuantifier :: Phantoms.TTerm OpenCypher.Quantifier -> Phantoms.TTerm OpenCypher.Atom
atomQuantifier x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "quantifier"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

atomVariable :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Atom
atomVariable x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Atom"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variable"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

caseAlternative :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.CaseAlternative
caseAlternative condition result =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm condition)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm result)}]}))

caseAlternativeCondition :: Phantoms.TTerm OpenCypher.CaseAlternative -> Phantoms.TTerm OpenCypher.Expression
caseAlternativeCondition x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
        Core.projectionField = (Core.Name "condition")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseAlternativeResult :: Phantoms.TTerm OpenCypher.CaseAlternative -> Phantoms.TTerm OpenCypher.Expression
caseAlternativeResult x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
        Core.projectionField = (Core.Name "result")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseAlternativeWithCondition :: Phantoms.TTerm OpenCypher.CaseAlternative -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.CaseAlternative
caseAlternativeWithCondition original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
              Core.projectionField = (Core.Name "result")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseAlternativeWithResult :: Phantoms.TTerm OpenCypher.CaseAlternative -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.CaseAlternative
caseAlternativeWithResult original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "condition"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseAlternative"),
              Core.projectionField = (Core.Name "condition")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpression :: Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm [OpenCypher.CaseAlternative] -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.CaseExpression
caseExpression expression alternatives else_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm alternatives)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm else_)}]}))

caseExpressionAlternatives :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm [OpenCypher.CaseAlternative]
caseExpressionAlternatives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionField = (Core.Name "alternatives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionElse :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression)
caseExpressionElse x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionField = (Core.Name "else")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionExpression :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression)
caseExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseExpressionWithAlternatives :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm [OpenCypher.CaseAlternative] -> Phantoms.TTerm OpenCypher.CaseExpression
caseExpressionWithAlternatives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseExpressionWithElse :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.CaseExpression
caseExpressionWithElse original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "alternatives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

caseExpressionWithExpression :: Phantoms.TTerm OpenCypher.CaseExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.CaseExpression
caseExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "alternatives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "alternatives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "else"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.CaseExpression"),
              Core.projectionField = (Core.Name "else")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

comparisonExpression :: Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm [OpenCypher.PartialComparisonExpression] -> Phantoms.TTerm OpenCypher.ComparisonExpression
comparisonExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

comparisonExpressionLeft :: Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression
comparisonExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

comparisonExpressionRight :: Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm [OpenCypher.PartialComparisonExpression]
comparisonExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

comparisonExpressionWithLeft :: Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm OpenCypher.ComparisonExpression
comparisonExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

comparisonExpressionWithRight :: Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm [OpenCypher.PartialComparisonExpression] -> Phantoms.TTerm OpenCypher.ComparisonExpression
comparisonExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

comparisonOperatorEq :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorEq =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eq"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGt :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorGt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorGte :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorGte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLt :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorLt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lt"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorLte :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorLte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lte"),
        Core.fieldTerm = Core.TermUnit}}))

comparisonOperatorNeq :: Phantoms.TTerm OpenCypher.ComparisonOperator
comparisonOperatorNeq =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ComparisonOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "neq"),
        Core.fieldTerm = Core.TermUnit}}))

create :: Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm OpenCypher.Create
create x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Create"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

delete :: Phantoms.TTerm Bool -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.Delete
delete detach expressions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Phantoms.unTTerm detach)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm expressions)}]}))

deleteDetach :: Phantoms.TTerm OpenCypher.Delete -> Phantoms.TTerm Bool
deleteDetach x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
        Core.projectionField = (Core.Name "detach")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deleteExpressions :: Phantoms.TTerm OpenCypher.Delete -> Phantoms.TTerm [OpenCypher.Expression]
deleteExpressions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
        Core.projectionField = (Core.Name "expressions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deleteWithDetach :: Phantoms.TTerm OpenCypher.Delete -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.Delete
deleteWithDetach original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
              Core.projectionField = (Core.Name "expressions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

deleteWithExpressions :: Phantoms.TTerm OpenCypher.Delete -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.Delete
deleteWithExpressions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "detach"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Delete"),
              Core.projectionField = (Core.Name "detach")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expressions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

existentialSubqueryPattern :: Phantoms.TTerm OpenCypher.PatternWhere -> Phantoms.TTerm OpenCypher.ExistentialSubquery
existentialSubqueryPattern x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ExistentialSubquery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pattern"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

existentialSubqueryRegular :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm OpenCypher.ExistentialSubquery
existentialSubqueryRegular x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ExistentialSubquery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

explicitProcedureInvocation :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocation name arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

explicitProcedureInvocationArguments :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm [OpenCypher.Expression]
explicitProcedureInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitProcedureInvocationName :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.QualifiedName
explicitProcedureInvocationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

explicitProcedureInvocationWithArguments :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

explicitProcedureInvocationWithName :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation
explicitProcedureInvocationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ExplicitProcedureInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

expression :: Phantoms.TTerm OpenCypher.OrExpression -> Phantoms.TTerm OpenCypher.Expression
expression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Expression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

filterExpression :: Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.FilterExpression
filterExpression idInColl where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Phantoms.unTTerm idInColl)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)}]}))

filterExpressionIdInColl :: Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm OpenCypher.IdInColl
filterExpressionIdInColl x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
        Core.projectionField = (Core.Name "idInColl")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

filterExpressionWhere :: Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm (Maybe OpenCypher.Where)
filterExpressionWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

filterExpressionWithIdInColl :: Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm OpenCypher.FilterExpression
filterExpressionWithIdInColl original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

filterExpressionWithWhere :: Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.FilterExpression
filterExpressionWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "idInColl"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FilterExpression"),
              Core.projectionField = (Core.Name "idInColl")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionInvocation :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm Bool -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.FunctionInvocation
functionInvocation name distinct arguments =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm arguments)}]}))

functionInvocationArguments :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm [OpenCypher.Expression]
functionInvocationArguments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionField = (Core.Name "arguments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionInvocationDistinct :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm Bool
functionInvocationDistinct x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionField = (Core.Name "distinct")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionInvocationName :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm OpenCypher.QualifiedName
functionInvocationName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

functionInvocationWithArguments :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.FunctionInvocation
functionInvocationWithArguments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

functionInvocationWithDistinct :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.FunctionInvocation
functionInvocationWithDistinct original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

functionInvocationWithName :: Phantoms.TTerm OpenCypher.FunctionInvocation -> Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm OpenCypher.FunctionInvocation
functionInvocationWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "arguments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.FunctionInvocation"),
              Core.projectionField = (Core.Name "arguments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

idInColl :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.IdInColl
idInColl variable expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

idInCollExpression :: Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm OpenCypher.Expression
idInCollExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

idInCollVariable :: Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm OpenCypher.Variable
idInCollVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

idInCollWithExpression :: Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.IdInColl
idInCollWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

idInCollWithVariable :: Phantoms.TTerm OpenCypher.IdInColl -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.IdInColl
idInCollWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.IdInColl"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

implicitProcedureInvocation :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm OpenCypher.ImplicitProcedureInvocation
implicitProcedureInvocation x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ImplicitProcedureInvocation"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

inQueryCall :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm (Maybe OpenCypher.YieldItems) -> Phantoms.TTerm OpenCypher.InQueryCall
inQueryCall call yieldItems =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTTerm call)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTTerm yieldItems)}]}))

inQueryCallCall :: Phantoms.TTerm OpenCypher.InQueryCall -> Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation
inQueryCallCall x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
        Core.projectionField = (Core.Name "call")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inQueryCallWithCall :: Phantoms.TTerm OpenCypher.InQueryCall -> Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.InQueryCall
inQueryCallWithCall original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
              Core.projectionField = (Core.Name "yieldItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inQueryCallWithYieldItems :: Phantoms.TTerm OpenCypher.InQueryCall -> Phantoms.TTerm (Maybe OpenCypher.YieldItems) -> Phantoms.TTerm OpenCypher.InQueryCall
inQueryCallWithYieldItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
              Core.projectionField = (Core.Name "call")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inQueryCallYieldItems :: Phantoms.TTerm OpenCypher.InQueryCall -> Phantoms.TTerm (Maybe OpenCypher.YieldItems)
inQueryCallYieldItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.InQueryCall"),
        Core.projectionField = (Core.Name "yieldItems")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValuePair :: Phantoms.TTerm OpenCypher.PropertyKeyName -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.KeyValuePair
keyValuePair key value =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm key)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm value)}]}))

keyValuePairKey :: Phantoms.TTerm OpenCypher.KeyValuePair -> Phantoms.TTerm OpenCypher.PropertyKeyName
keyValuePairKey x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
        Core.projectionField = (Core.Name "key")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValuePairValue :: Phantoms.TTerm OpenCypher.KeyValuePair -> Phantoms.TTerm OpenCypher.Expression
keyValuePairValue x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
        Core.projectionField = (Core.Name "value")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

keyValuePairWithKey :: Phantoms.TTerm OpenCypher.KeyValuePair -> Phantoms.TTerm OpenCypher.PropertyKeyName -> Phantoms.TTerm OpenCypher.KeyValuePair
keyValuePairWithKey original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
              Core.projectionField = (Core.Name "value")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

keyValuePairWithValue :: Phantoms.TTerm OpenCypher.KeyValuePair -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.KeyValuePair
keyValuePairWithValue original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "key"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.KeyValuePair"),
              Core.projectionField = (Core.Name "key")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "value"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

limit :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Limit
limit x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Limit"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

listComprehension :: Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.ListComprehension
listComprehension left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

listComprehensionLeft :: Phantoms.TTerm OpenCypher.ListComprehension -> Phantoms.TTerm OpenCypher.FilterExpression
listComprehensionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listComprehensionRight :: Phantoms.TTerm OpenCypher.ListComprehension -> Phantoms.TTerm (Maybe OpenCypher.Expression)
listComprehensionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

listComprehensionWithLeft :: Phantoms.TTerm OpenCypher.ListComprehension -> Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm OpenCypher.ListComprehension
listComprehensionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

listComprehensionWithRight :: Phantoms.TTerm OpenCypher.ListComprehension -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.ListComprehension
listComprehensionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ListComprehension"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

listLiteral :: Phantoms.TTerm [OpenCypher.Expression] -> Phantoms.TTerm OpenCypher.ListLiteral
listLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ListLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

listOperatorExpressionOrPropertyLookupList :: Phantoms.TTerm OpenCypher.ListOperatorExpression -> Phantoms.TTerm OpenCypher.ListOperatorExpressionOrPropertyLookup
listOperatorExpressionOrPropertyLookupList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpressionOrPropertyLookup"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listOperatorExpressionOrPropertyLookupProperty :: Phantoms.TTerm OpenCypher.PropertyLookup -> Phantoms.TTerm OpenCypher.ListOperatorExpressionOrPropertyLookup
listOperatorExpressionOrPropertyLookupProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpressionOrPropertyLookup"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listOperatorExpressionRange :: Phantoms.TTerm OpenCypher.RangeExpression -> Phantoms.TTerm OpenCypher.ListOperatorExpression
listOperatorExpressionRange x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "range"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listOperatorExpressionSingle :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.ListOperatorExpression
listOperatorExpressionSingle x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ListOperatorExpression"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

listPredicateExpression :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.ListPredicateExpression
listPredicateExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ListPredicateExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

literalBoolean :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.Literal
literalBoolean x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalList :: Phantoms.TTerm OpenCypher.ListLiteral -> Phantoms.TTerm OpenCypher.Literal
literalList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalMap :: Phantoms.TTerm OpenCypher.MapLiteral -> Phantoms.TTerm OpenCypher.Literal
literalMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalNull :: Phantoms.TTerm OpenCypher.Literal
literalNull =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = Core.TermUnit}}))

literalNumber :: Phantoms.TTerm OpenCypher.NumberLiteral -> Phantoms.TTerm OpenCypher.Literal
literalNumber x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "number"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

literalString :: Phantoms.TTerm OpenCypher.StringLiteral -> Phantoms.TTerm OpenCypher.Literal
literalString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Literal"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

mapLiteral :: Phantoms.TTerm [OpenCypher.KeyValuePair] -> Phantoms.TTerm OpenCypher.MapLiteral
mapLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.MapLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

match :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.Match
match optional pattern where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
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

matchOptional :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm Bool
matchOptional x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionField = (Core.Name "optional")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchOrCreateCreate :: Phantoms.TTerm OpenCypher.MatchOrCreate
matchOrCreateCreate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MatchOrCreate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "create"),
        Core.fieldTerm = Core.TermUnit}}))

matchOrCreateMatch :: Phantoms.TTerm OpenCypher.MatchOrCreate
matchOrCreateMatch =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MatchOrCreate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = Core.TermUnit}}))

matchPattern :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm OpenCypher.Pattern
matchPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchWhere :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm (Maybe OpenCypher.Where)
matchWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

matchWithOptional :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.Match
matchWithOptional original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithPattern :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm OpenCypher.Match
matchWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

matchWithWhere :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.Match
matchWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "optional"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "optional")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Match"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

merge :: Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm [OpenCypher.MergeAction] -> Phantoms.TTerm OpenCypher.Merge
merge patternPart actions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Phantoms.unTTerm patternPart)},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Phantoms.unTTerm actions)}]}))

mergeAction :: Phantoms.TTerm OpenCypher.MatchOrCreate -> Phantoms.TTerm OpenCypher.Set -> Phantoms.TTerm OpenCypher.MergeAction
mergeAction action set =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Phantoms.unTTerm action)},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Phantoms.unTTerm set)}]}))

mergeActionAction :: Phantoms.TTerm OpenCypher.MergeAction -> Phantoms.TTerm OpenCypher.MatchOrCreate
mergeActionAction x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
        Core.projectionField = (Core.Name "action")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mergeActionSet :: Phantoms.TTerm OpenCypher.MergeAction -> Phantoms.TTerm OpenCypher.Set
mergeActionSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
        Core.projectionField = (Core.Name "set")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mergeActionWithAction :: Phantoms.TTerm OpenCypher.MergeAction -> Phantoms.TTerm OpenCypher.MatchOrCreate -> Phantoms.TTerm OpenCypher.MergeAction
mergeActionWithAction original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
              Core.projectionField = (Core.Name "set")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

mergeActionWithSet :: Phantoms.TTerm OpenCypher.MergeAction -> Phantoms.TTerm OpenCypher.Set -> Phantoms.TTerm OpenCypher.MergeAction
mergeActionWithSet original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "action"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MergeAction"),
              Core.projectionField = (Core.Name "action")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "set"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mergeActions :: Phantoms.TTerm OpenCypher.Merge -> Phantoms.TTerm [OpenCypher.MergeAction]
mergeActions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
        Core.projectionField = (Core.Name "actions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mergePatternPart :: Phantoms.TTerm OpenCypher.Merge -> Phantoms.TTerm OpenCypher.PatternPart
mergePatternPart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
        Core.projectionField = (Core.Name "patternPart")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

mergeWithActions :: Phantoms.TTerm OpenCypher.Merge -> Phantoms.TTerm [OpenCypher.MergeAction] -> Phantoms.TTerm OpenCypher.Merge
mergeWithActions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
              Core.projectionField = (Core.Name "patternPart")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

mergeWithPatternPart :: Phantoms.TTerm OpenCypher.Merge -> Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm OpenCypher.Merge
mergeWithPatternPart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "patternPart"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "actions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Merge"),
              Core.projectionField = (Core.Name "actions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiPartQuery :: Phantoms.TTerm [OpenCypher.WithClause] -> Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm OpenCypher.MultiPartQuery
multiPartQuery with body =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm body)}]}))

multiPartQueryBody :: Phantoms.TTerm OpenCypher.MultiPartQuery -> Phantoms.TTerm OpenCypher.SinglePartQuery
multiPartQueryBody x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
        Core.projectionField = (Core.Name "body")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPartQueryWith :: Phantoms.TTerm OpenCypher.MultiPartQuery -> Phantoms.TTerm [OpenCypher.WithClause]
multiPartQueryWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
        Core.projectionField = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiPartQueryWithBody :: Phantoms.TTerm OpenCypher.MultiPartQuery -> Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm OpenCypher.MultiPartQuery
multiPartQueryWithBody original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiPartQueryWithWith :: Phantoms.TTerm OpenCypher.MultiPartQuery -> Phantoms.TTerm [OpenCypher.WithClause] -> Phantoms.TTerm OpenCypher.MultiPartQuery
multiPartQueryWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiPartQuery"),
              Core.projectionField = (Core.Name "body")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiplyDivideModuloExpression :: Phantoms.TTerm OpenCypher.PowerOfExpression -> Phantoms.TTerm [OpenCypher.MultiplyDivideModuloRightHandSide] -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

multiplyDivideModuloExpressionLeft :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm OpenCypher.PowerOfExpression
multiplyDivideModuloExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyDivideModuloExpressionRight :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm [OpenCypher.MultiplyDivideModuloRightHandSide]
multiplyDivideModuloExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyDivideModuloExpressionWithLeft :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm OpenCypher.PowerOfExpression -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

multiplyDivideModuloExpressionWithRight :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression -> Phantoms.TTerm [OpenCypher.MultiplyDivideModuloRightHandSide] -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloExpression
multiplyDivideModuloExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiplyDivideModuloOperatorDivide :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorDivide =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "divide"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloOperatorModulo :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorModulo =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "modulo"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloOperatorMultiply :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloOperatorMultiply =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiply"),
        Core.fieldTerm = Core.TermUnit}}))

multiplyDivideModuloRightHandSide :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator -> Phantoms.TTerm OpenCypher.PowerOfExpression -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSide operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

multiplyDivideModuloRightHandSideExpression :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TTerm OpenCypher.PowerOfExpression
multiplyDivideModuloRightHandSideExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyDivideModuloRightHandSideOperator :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator
multiplyDivideModuloRightHandSideOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

multiplyDivideModuloRightHandSideWithExpression :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TTerm OpenCypher.PowerOfExpression -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSideWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

multiplyDivideModuloRightHandSideWithOperator :: Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloOperator -> Phantoms.TTerm OpenCypher.MultiplyDivideModuloRightHandSide
multiplyDivideModuloRightHandSideWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodeLabel :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.NodeLabel
nodeLabel x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NodeLabel"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nodeLabels :: Phantoms.TTerm [OpenCypher.NodeLabel] -> Phantoms.TTerm OpenCypher.NodeLabels
nodeLabels x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NodeLabels"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nodePattern :: Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TTerm (Maybe OpenCypher.Properties) -> Phantoms.TTerm OpenCypher.NodePattern
nodePattern variable labels properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

nodePatternChain :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm [OpenCypher.PatternElementChain] -> Phantoms.TTerm OpenCypher.NodePatternChain
nodePatternChain nodePattern chain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTTerm nodePattern)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTTerm chain)}]}))

nodePatternChainChain :: Phantoms.TTerm OpenCypher.NodePatternChain -> Phantoms.TTerm [OpenCypher.PatternElementChain]
nodePatternChainChain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
        Core.projectionField = (Core.Name "chain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternChainNodePattern :: Phantoms.TTerm OpenCypher.NodePatternChain -> Phantoms.TTerm OpenCypher.NodePattern
nodePatternChainNodePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
        Core.projectionField = (Core.Name "nodePattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternChainWithChain :: Phantoms.TTerm OpenCypher.NodePatternChain -> Phantoms.TTerm [OpenCypher.PatternElementChain] -> Phantoms.TTerm OpenCypher.NodePatternChain
nodePatternChainWithChain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
              Core.projectionField = (Core.Name "nodePattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodePatternChainWithNodePattern :: Phantoms.TTerm OpenCypher.NodePatternChain -> Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm OpenCypher.NodePatternChain
nodePatternChainWithNodePattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePatternChain"),
              Core.projectionField = (Core.Name "chain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodePatternLabels :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels)
nodePatternLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionField = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternProperties :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.Properties)
nodePatternProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternVariable :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.Variable)
nodePatternVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nodePatternWithLabels :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TTerm OpenCypher.NodePattern
nodePatternWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nodePatternWithProperties :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.Properties) -> Phantoms.TTerm OpenCypher.NodePattern
nodePatternWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nodePatternWithVariable :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.NodePattern
nodePatternWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NodePattern"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nonArithmeticOperatorExpression :: Phantoms.TTerm OpenCypher.Atom -> Phantoms.TTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup] -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpression atom listsAndLookups labels =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Phantoms.unTTerm listsAndLookups)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)}]}))

nonArithmeticOperatorExpressionAtom :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm OpenCypher.Atom
nonArithmeticOperatorExpressionAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionField = (Core.Name "atom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonArithmeticOperatorExpressionLabels :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels)
nonArithmeticOperatorExpressionLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionField = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonArithmeticOperatorExpressionListsAndLookups :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup]
nonArithmeticOperatorExpressionListsAndLookups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
        Core.projectionField = (Core.Name "listsAndLookups")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

nonArithmeticOperatorExpressionWithAtom :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm OpenCypher.Atom -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "listsAndLookups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nonArithmeticOperatorExpressionWithLabels :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm (Maybe OpenCypher.NodeLabels) -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "listsAndLookups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

nonArithmeticOperatorExpressionWithListsAndLookups :: Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm [OpenCypher.ListOperatorExpressionOrPropertyLookup] -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression
nonArithmeticOperatorExpressionWithListsAndLookups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "listsAndLookups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NonArithmeticOperatorExpression"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

notExpression :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm OpenCypher.NotExpression
notExpression not expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTTerm not)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

notExpressionExpression :: Phantoms.TTerm OpenCypher.NotExpression -> Phantoms.TTerm OpenCypher.ComparisonExpression
notExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notExpressionNot :: Phantoms.TTerm OpenCypher.NotExpression -> Phantoms.TTerm Bool
notExpressionNot x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
        Core.projectionField = (Core.Name "not")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

notExpressionWithExpression :: Phantoms.TTerm OpenCypher.NotExpression -> Phantoms.TTerm OpenCypher.ComparisonExpression -> Phantoms.TTerm OpenCypher.NotExpression
notExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
              Core.projectionField = (Core.Name "not")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

notExpressionWithNot :: Phantoms.TTerm OpenCypher.NotExpression -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.NotExpression
notExpressionWithNot original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "not"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.NotExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

nullPredicateExpression :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.NullPredicateExpression
nullPredicateExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.NullPredicateExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

numberLiteralDouble :: Phantoms.TTerm Double -> Phantoms.TTerm OpenCypher.NumberLiteral
numberLiteralDouble x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.NumberLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

numberLiteralInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm OpenCypher.NumberLiteral
numberLiteralInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.NumberLiteral"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

orExpression :: Phantoms.TTerm [OpenCypher.XorExpression] -> Phantoms.TTerm OpenCypher.OrExpression
orExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.OrExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

order :: Phantoms.TTerm [OpenCypher.SortItem] -> Phantoms.TTerm OpenCypher.Order
order x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Order"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

parameterInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm OpenCypher.Parameter
parameterInteger x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Parameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

parameterSymbolic :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.Parameter
parameterSymbolic x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Parameter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "symbolic"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

parenthesizedExpression :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.ParenthesizedExpression
parenthesizedExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ParenthesizedExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

partialComparisonExpression :: Phantoms.TTerm OpenCypher.ComparisonOperator -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm OpenCypher.PartialComparisonExpression
partialComparisonExpression operator right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

partialComparisonExpressionOperator :: Phantoms.TTerm OpenCypher.PartialComparisonExpression -> Phantoms.TTerm OpenCypher.ComparisonOperator
partialComparisonExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

partialComparisonExpressionRight :: Phantoms.TTerm OpenCypher.PartialComparisonExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression
partialComparisonExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

partialComparisonExpressionWithOperator :: Phantoms.TTerm OpenCypher.PartialComparisonExpression -> Phantoms.TTerm OpenCypher.ComparisonOperator -> Phantoms.TTerm OpenCypher.PartialComparisonExpression
partialComparisonExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

partialComparisonExpressionWithRight :: Phantoms.TTerm OpenCypher.PartialComparisonExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm OpenCypher.PartialComparisonExpression
partialComparisonExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PartialComparisonExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

pattern :: Phantoms.TTerm [OpenCypher.PatternPart] -> Phantoms.TTerm OpenCypher.Pattern
pattern x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Pattern"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

patternComprehension :: Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.PatternComprehension
patternComprehension variable pattern where_ right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

patternComprehensionPattern :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm OpenCypher.RelationshipsPattern
patternComprehensionPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternComprehensionRight :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm OpenCypher.Expression
patternComprehensionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternComprehensionVariable :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm (Maybe OpenCypher.Variable)
patternComprehensionVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternComprehensionWhere :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm (Maybe OpenCypher.Where)
patternComprehensionWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternComprehensionWithPattern :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm OpenCypher.PatternComprehension
patternComprehensionWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternComprehensionWithRight :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.PatternComprehension
patternComprehensionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternComprehensionWithVariable :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.PatternComprehension
patternComprehensionWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternComprehensionWithWhere :: Phantoms.TTerm OpenCypher.PatternComprehension -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.PatternComprehension
patternComprehensionWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternComprehension"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternElementChain :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm OpenCypher.PatternElementChain
patternElementChain relationship node =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Phantoms.unTTerm relationship)},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Phantoms.unTTerm node)}]}))

patternElementChainNode :: Phantoms.TTerm OpenCypher.PatternElementChain -> Phantoms.TTerm OpenCypher.NodePattern
patternElementChainNode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
        Core.projectionField = (Core.Name "node")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternElementChainRelationship :: Phantoms.TTerm OpenCypher.PatternElementChain -> Phantoms.TTerm OpenCypher.RelationshipPattern
patternElementChainRelationship x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
        Core.projectionField = (Core.Name "relationship")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternElementChainWithNode :: Phantoms.TTerm OpenCypher.PatternElementChain -> Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm OpenCypher.PatternElementChain
patternElementChainWithNode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
              Core.projectionField = (Core.Name "relationship")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternElementChainWithRelationship :: Phantoms.TTerm OpenCypher.PatternElementChain -> Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm OpenCypher.PatternElementChain
patternElementChainWithRelationship original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "relationship"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "node"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElementChain"),
              Core.projectionField = (Core.Name "node")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternElementChained :: Phantoms.TTerm OpenCypher.NodePatternChain -> Phantoms.TTerm OpenCypher.PatternElement
patternElementChained x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "chained"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternElementParenthesized :: Phantoms.TTerm OpenCypher.PatternElement -> Phantoms.TTerm OpenCypher.PatternElement
patternElementParenthesized x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternElement"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parenthesized"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

patternPart :: Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.AnonymousPatternPart -> Phantoms.TTerm OpenCypher.PatternPart
patternPart variable pattern =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)}]}))

patternPartPattern :: Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm OpenCypher.AnonymousPatternPart
patternPartPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternPartVariable :: Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm (Maybe OpenCypher.Variable)
patternPartVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternPartWithPattern :: Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm OpenCypher.AnonymousPatternPart -> Phantoms.TTerm OpenCypher.PatternPart
patternPartWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

patternPartWithVariable :: Phantoms.TTerm OpenCypher.PatternPart -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.PatternPart
patternPartWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternPart"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternPredicate :: Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm OpenCypher.PatternPredicate
patternPredicate x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PatternPredicate"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

patternWhere :: Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.PatternWhere
patternWhere pattern where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm pattern)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)}]}))

patternWherePattern :: Phantoms.TTerm OpenCypher.PatternWhere -> Phantoms.TTerm OpenCypher.Pattern
patternWherePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
        Core.projectionField = (Core.Name "pattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternWhereWhere :: Phantoms.TTerm OpenCypher.PatternWhere -> Phantoms.TTerm (Maybe OpenCypher.Where)
patternWhereWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

patternWhereWithPattern :: Phantoms.TTerm OpenCypher.PatternWhere -> Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm OpenCypher.PatternWhere
patternWhereWithPattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

patternWhereWithWhere :: Phantoms.TTerm OpenCypher.PatternWhere -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.PatternWhere
patternWhereWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "pattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PatternWhere"),
              Core.projectionField = (Core.Name "pattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

powerOfExpression :: Phantoms.TTerm [OpenCypher.UnaryAddOrSubtractExpression] -> Phantoms.TTerm OpenCypher.PowerOfExpression
powerOfExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PowerOfExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

procedureInvocationExplicit :: Phantoms.TTerm OpenCypher.ExplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.ProcedureInvocation
procedureInvocationExplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureInvocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "explicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

procedureInvocationImplicit :: Phantoms.TTerm OpenCypher.ImplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.ProcedureInvocation
procedureInvocationImplicit x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureInvocation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "implicit"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

procedureResultField :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.ProcedureResultField
procedureResultField x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.ProcedureResultField"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

projectionBody :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm (Maybe OpenCypher.Order) -> Phantoms.TTerm (Maybe OpenCypher.Skip) -> Phantoms.TTerm (Maybe OpenCypher.Limit) -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBody distinct projectionItems order skip limit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm distinct)},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Phantoms.unTTerm projectionItems)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm order)},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Phantoms.unTTerm skip)},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Phantoms.unTTerm limit)}]}))

projectionBodyDistinct :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm Bool
projectionBodyDistinct x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionField = (Core.Name "distinct")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionBodyLimit :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Limit)
projectionBodyLimit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionField = (Core.Name "limit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionBodyOrder :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Order)
projectionBodyOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionBodyProjectionItems :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm OpenCypher.ProjectionItems
projectionBodyProjectionItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionField = (Core.Name "projectionItems")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionBodySkip :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Skip)
projectionBodySkip x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
        Core.projectionField = (Core.Name "skip")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionBodyWithDistinct :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBodyWithDistinct original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionBodyWithLimit :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Limit) -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBodyWithLimit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionBodyWithOrder :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Order) -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBodyWithOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionBodyWithProjectionItems :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBodyWithProjectionItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "skip")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionBodyWithSkip :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Skip) -> Phantoms.TTerm OpenCypher.ProjectionBody
projectionBodyWithSkip original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "distinct"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "distinct")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "projectionItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "projectionItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "skip"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "limit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionBody"),
              Core.projectionField = (Core.Name "limit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionItem :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.ProjectionItem
projectionItem expression variable =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)}]}))

projectionItemExpression :: Phantoms.TTerm OpenCypher.ProjectionItem -> Phantoms.TTerm OpenCypher.Expression
projectionItemExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionItemVariable :: Phantoms.TTerm OpenCypher.ProjectionItem -> Phantoms.TTerm (Maybe OpenCypher.Variable)
projectionItemVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionItemWithExpression :: Phantoms.TTerm OpenCypher.ProjectionItem -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.ProjectionItem
projectionItemWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

projectionItemWithVariable :: Phantoms.TTerm OpenCypher.ProjectionItem -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.ProjectionItem
projectionItemWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItem"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionItems :: Phantoms.TTerm Bool -> Phantoms.TTerm [OpenCypher.ProjectionItem] -> Phantoms.TTerm OpenCypher.ProjectionItems
projectionItems star explicit =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Phantoms.unTTerm star)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTTerm explicit)}]}))

projectionItemsExplicit :: Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm [OpenCypher.ProjectionItem]
projectionItemsExplicit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
        Core.projectionField = (Core.Name "explicit")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionItemsStar :: Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm Bool
projectionItemsStar x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
        Core.projectionField = (Core.Name "star")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

projectionItemsWithExplicit :: Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm [OpenCypher.ProjectionItem] -> Phantoms.TTerm OpenCypher.ProjectionItems
projectionItemsWithExplicit original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
              Core.projectionField = (Core.Name "star")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

projectionItemsWithStar :: Phantoms.TTerm OpenCypher.ProjectionItems -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.ProjectionItems
projectionItemsWithStar original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "star"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "explicit"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.ProjectionItems"),
              Core.projectionField = (Core.Name "explicit")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertiesMap :: Phantoms.TTerm OpenCypher.MapLiteral -> Phantoms.TTerm OpenCypher.Properties
propertiesMap x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Properties"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "map"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertiesParameter :: Phantoms.TTerm OpenCypher.Parameter -> Phantoms.TTerm OpenCypher.Properties
propertiesParameter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Properties"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "parameter"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

propertyEquals :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.PropertyEquals
propertyEquals lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

propertyEqualsLhs :: Phantoms.TTerm OpenCypher.PropertyEquals -> Phantoms.TTerm OpenCypher.PropertyExpression
propertyEqualsLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyEqualsRhs :: Phantoms.TTerm OpenCypher.PropertyEquals -> Phantoms.TTerm OpenCypher.Expression
propertyEqualsRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyEqualsWithLhs :: Phantoms.TTerm OpenCypher.PropertyEquals -> Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm OpenCypher.PropertyEquals
propertyEqualsWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyEqualsWithRhs :: Phantoms.TTerm OpenCypher.PropertyEquals -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.PropertyEquals
propertyEqualsWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyEquals"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyExpression :: Phantoms.TTerm OpenCypher.Atom -> Phantoms.TTerm [OpenCypher.PropertyLookup] -> Phantoms.TTerm OpenCypher.PropertyExpression
propertyExpression atom lookups =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTTerm atom)},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Phantoms.unTTerm lookups)}]}))

propertyExpressionAtom :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm OpenCypher.Atom
propertyExpressionAtom x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
        Core.projectionField = (Core.Name "atom")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExpressionLookups :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm [OpenCypher.PropertyLookup]
propertyExpressionLookups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
        Core.projectionField = (Core.Name "lookups")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

propertyExpressionWithAtom :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm OpenCypher.Atom -> Phantoms.TTerm OpenCypher.PropertyExpression
propertyExpressionWithAtom original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
              Core.projectionField = (Core.Name "lookups")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

propertyExpressionWithLookups :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm [OpenCypher.PropertyLookup] -> Phantoms.TTerm OpenCypher.PropertyExpression
propertyExpressionWithLookups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "atom"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.PropertyExpression"),
              Core.projectionField = (Core.Name "atom")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lookups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

propertyKeyName :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.PropertyKeyName
propertyKeyName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PropertyKeyName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

propertyLookup :: Phantoms.TTerm OpenCypher.PropertyKeyName -> Phantoms.TTerm OpenCypher.PropertyLookup
propertyLookup x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.PropertyLookup"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

qualifiedName :: Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.QualifiedName
qualifiedName namespace local =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm local)}]}))

qualifiedNameLocal :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm String
qualifiedNameLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
        Core.projectionField = (Core.Name "local")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameNamespace :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm String
qualifiedNameNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameWithLocal :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.QualifiedName
qualifiedNameWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedNameWithNamespace :: Phantoms.TTerm OpenCypher.QualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.QualifiedName
qualifiedNameWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.QualifiedName"),
              Core.projectionField = (Core.Name "local")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

quantifier :: Phantoms.TTerm OpenCypher.QuantifierOperator -> Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm OpenCypher.Quantifier
quantifier operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

quantifierExpression :: Phantoms.TTerm OpenCypher.Quantifier -> Phantoms.TTerm OpenCypher.FilterExpression
quantifierExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quantifierOperator :: Phantoms.TTerm OpenCypher.Quantifier -> Phantoms.TTerm OpenCypher.QuantifierOperator
quantifierOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

quantifierOperatorAll :: Phantoms.TTerm OpenCypher.QuantifierOperator
quantifierOperatorAll =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "all"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorAny :: Phantoms.TTerm OpenCypher.QuantifierOperator
quantifierOperatorAny =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorNone :: Phantoms.TTerm OpenCypher.QuantifierOperator
quantifierOperatorNone =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierOperatorSingle :: Phantoms.TTerm OpenCypher.QuantifierOperator
quantifierOperatorSingle =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.QuantifierOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "single"),
        Core.fieldTerm = Core.TermUnit}}))

quantifierWithExpression :: Phantoms.TTerm OpenCypher.Quantifier -> Phantoms.TTerm OpenCypher.FilterExpression -> Phantoms.TTerm OpenCypher.Quantifier
quantifierWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

quantifierWithOperator :: Phantoms.TTerm OpenCypher.Quantifier -> Phantoms.TTerm OpenCypher.QuantifierOperator -> Phantoms.TTerm OpenCypher.Quantifier
quantifierWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Quantifier"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

queryRegular :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm OpenCypher.Query
queryRegular x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "regular"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

queryStandalone :: Phantoms.TTerm OpenCypher.StandaloneCall -> Phantoms.TTerm OpenCypher.Query
queryStandalone x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.Query"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "standalone"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

rangeExpression :: Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.RangeExpression
rangeExpression start end =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm end)}]}))

rangeExpressionEnd :: Phantoms.TTerm OpenCypher.RangeExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression)
rangeExpressionEnd x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
        Core.projectionField = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeExpressionStart :: Phantoms.TTerm OpenCypher.RangeExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression)
rangeExpressionStart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
        Core.projectionField = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeExpressionWithEnd :: Phantoms.TTerm OpenCypher.RangeExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.RangeExpression
rangeExpressionWithEnd original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeExpressionWithStart :: Phantoms.TTerm OpenCypher.RangeExpression -> Phantoms.TTerm (Maybe OpenCypher.Expression) -> Phantoms.TTerm OpenCypher.RangeExpression
rangeExpressionWithStart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeExpression"),
              Core.projectionField = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rangeLiteral :: Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm OpenCypher.RangeLiteral
rangeLiteral start end =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm start)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm end)}]}))

rangeLiteralEnd :: Phantoms.TTerm OpenCypher.RangeLiteral -> Phantoms.TTerm (Maybe Integer)
rangeLiteralEnd x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
        Core.projectionField = (Core.Name "end")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeLiteralStart :: Phantoms.TTerm OpenCypher.RangeLiteral -> Phantoms.TTerm (Maybe Integer)
rangeLiteralStart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
        Core.projectionField = (Core.Name "start")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rangeLiteralWithEnd :: Phantoms.TTerm OpenCypher.RangeLiteral -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm OpenCypher.RangeLiteral
rangeLiteralWithEnd original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
              Core.projectionField = (Core.Name "start")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

rangeLiteralWithStart :: Phantoms.TTerm OpenCypher.RangeLiteral -> Phantoms.TTerm (Maybe Integer) -> Phantoms.TTerm OpenCypher.RangeLiteral
rangeLiteralWithStart original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "start"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "end"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RangeLiteral"),
              Core.projectionField = (Core.Name "end")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

readingClauseInQueryCall :: Phantoms.TTerm OpenCypher.InQueryCall -> Phantoms.TTerm OpenCypher.ReadingClause
readingClauseInQueryCall x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inQueryCall"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

readingClauseMatch :: Phantoms.TTerm OpenCypher.Match -> Phantoms.TTerm OpenCypher.ReadingClause
readingClauseMatch x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "match"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

readingClauseUnwind :: Phantoms.TTerm OpenCypher.Unwind -> Phantoms.TTerm OpenCypher.ReadingClause
readingClauseUnwind x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.ReadingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unwind"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

regularQuery :: Phantoms.TTerm OpenCypher.SingleQuery -> Phantoms.TTerm [OpenCypher.Union] -> Phantoms.TTerm OpenCypher.RegularQuery
regularQuery head rest =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm head)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm rest)}]}))

regularQueryHead :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm OpenCypher.SingleQuery
regularQueryHead x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
        Core.projectionField = (Core.Name "head")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

regularQueryRest :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm [OpenCypher.Union]
regularQueryRest x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
        Core.projectionField = (Core.Name "rest")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

regularQueryWithHead :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm OpenCypher.SingleQuery -> Phantoms.TTerm OpenCypher.RegularQuery
regularQueryWithHead original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
              Core.projectionField = (Core.Name "rest")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

regularQueryWithRest :: Phantoms.TTerm OpenCypher.RegularQuery -> Phantoms.TTerm [OpenCypher.Union] -> Phantoms.TTerm OpenCypher.RegularQuery
regularQueryWithRest original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "head"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RegularQuery"),
              Core.projectionField = (Core.Name "head")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rest"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relTypeName :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.RelTypeName
relTypeName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.RelTypeName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

relationshipDetail :: Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm (Maybe OpenCypher.RelationshipTypes) -> Phantoms.TTerm (Maybe OpenCypher.RangeLiteral) -> Phantoms.TTerm (Maybe OpenCypher.Properties) -> Phantoms.TTerm OpenCypher.RelationshipDetail
relationshipDetail variable types range properties =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm range)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm properties)}]}))

relationshipDetailProperties :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.Properties)
relationshipDetailProperties x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionField = (Core.Name "properties")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipDetailRange :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.RangeLiteral)
relationshipDetailRange x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionField = (Core.Name "range")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipDetailTypes :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.RelationshipTypes)
relationshipDetailTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionField = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipDetailVariable :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.Variable)
relationshipDetailVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipDetailWithProperties :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.Properties) -> Phantoms.TTerm OpenCypher.RelationshipDetail
relationshipDetailWithProperties original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationshipDetailWithRange :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.RangeLiteral) -> Phantoms.TTerm OpenCypher.RelationshipDetail
relationshipDetailWithRange original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationshipDetailWithTypes :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.RelationshipTypes) -> Phantoms.TTerm OpenCypher.RelationshipDetail
relationshipDetailWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationshipDetailWithVariable :: Phantoms.TTerm OpenCypher.RelationshipDetail -> Phantoms.TTerm (Maybe OpenCypher.Variable) -> Phantoms.TTerm OpenCypher.RelationshipDetail
relationshipDetailWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "range"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "range")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "properties"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipDetail"),
              Core.projectionField = (Core.Name "properties")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationshipPattern :: Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe OpenCypher.RelationshipDetail) -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.RelationshipPattern
relationshipPattern leftArrow detail rightArrow =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Phantoms.unTTerm leftArrow)},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Phantoms.unTTerm detail)},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Phantoms.unTTerm rightArrow)}]}))

relationshipPatternDetail :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm (Maybe OpenCypher.RelationshipDetail)
relationshipPatternDetail x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionField = (Core.Name "detail")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipPatternLeftArrow :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm Bool
relationshipPatternLeftArrow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionField = (Core.Name "leftArrow")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipPatternRightArrow :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm Bool
relationshipPatternRightArrow x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
        Core.projectionField = (Core.Name "rightArrow")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipPatternWithDetail :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm (Maybe OpenCypher.RelationshipDetail) -> Phantoms.TTerm OpenCypher.RelationshipPattern
relationshipPatternWithDetail original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "leftArrow")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "rightArrow")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationshipPatternWithLeftArrow :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.RelationshipPattern
relationshipPatternWithLeftArrow original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "detail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "rightArrow")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

relationshipPatternWithRightArrow :: Phantoms.TTerm OpenCypher.RelationshipPattern -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.RelationshipPattern
relationshipPatternWithRightArrow original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "leftArrow"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "leftArrow")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "detail"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipPattern"),
              Core.projectionField = (Core.Name "detail")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rightArrow"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationshipTypes :: Phantoms.TTerm [OpenCypher.RelTypeName] -> Phantoms.TTerm OpenCypher.RelationshipTypes
relationshipTypes x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipTypes"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

relationshipsPattern :: Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm [OpenCypher.PatternElementChain] -> Phantoms.TTerm OpenCypher.RelationshipsPattern
relationshipsPattern nodePattern chain =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTTerm nodePattern)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTTerm chain)}]}))

relationshipsPatternChain :: Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm [OpenCypher.PatternElementChain]
relationshipsPatternChain x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
        Core.projectionField = (Core.Name "chain")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipsPatternNodePattern :: Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm OpenCypher.NodePattern
relationshipsPatternNodePattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
        Core.projectionField = (Core.Name "nodePattern")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

relationshipsPatternWithChain :: Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm [OpenCypher.PatternElementChain] -> Phantoms.TTerm OpenCypher.RelationshipsPattern
relationshipsPatternWithChain original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
              Core.projectionField = (Core.Name "nodePattern")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

relationshipsPatternWithNodePattern :: Phantoms.TTerm OpenCypher.RelationshipsPattern -> Phantoms.TTerm OpenCypher.NodePattern -> Phantoms.TTerm OpenCypher.RelationshipsPattern
relationshipsPatternWithNodePattern original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodePattern"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "chain"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.RelationshipsPattern"),
              Core.projectionField = (Core.Name "chain")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

remove :: Phantoms.TTerm [OpenCypher.RemoveItem] -> Phantoms.TTerm OpenCypher.Remove
remove x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Remove"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

removeItemProperty :: Phantoms.TTerm OpenCypher.PropertyExpression -> Phantoms.TTerm OpenCypher.RemoveItem
removeItemProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.RemoveItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

removeItemVariableLabels :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.RemoveItem
removeItemVariableLabels x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.RemoveItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableLabels"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

return :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm OpenCypher.Return
return x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Return"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

set :: Phantoms.TTerm [OpenCypher.SetItem] -> Phantoms.TTerm OpenCypher.Set
set x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Set"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

setItemProperty :: Phantoms.TTerm OpenCypher.PropertyEquals -> Phantoms.TTerm OpenCypher.SetItem
setItemProperty x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "property"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

setItemVariableEqual :: Phantoms.TTerm OpenCypher.VariableEquals -> Phantoms.TTerm OpenCypher.SetItem
setItemVariableEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

setItemVariableLabels :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.SetItem
setItemVariableLabels x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableLabels"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

setItemVariablePlusEqual :: Phantoms.TTerm OpenCypher.VariablePlusEquals -> Phantoms.TTerm OpenCypher.SetItem
setItemVariablePlusEqual x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SetItem"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variablePlusEqual"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

singlePartQuery :: Phantoms.TTerm [OpenCypher.ReadingClause] -> Phantoms.TTerm [OpenCypher.UpdatingClause] -> Phantoms.TTerm (Maybe OpenCypher.Return) -> Phantoms.TTerm OpenCypher.SinglePartQuery
singlePartQuery reading updating return =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTTerm reading)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTTerm updating)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm return)}]}))

singlePartQueryReading :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm [OpenCypher.ReadingClause]
singlePartQueryReading x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionField = (Core.Name "reading")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singlePartQueryReturn :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm (Maybe OpenCypher.Return)
singlePartQueryReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionField = (Core.Name "return")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singlePartQueryUpdating :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm [OpenCypher.UpdatingClause]
singlePartQueryUpdating x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
        Core.projectionField = (Core.Name "updating")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

singlePartQueryWithReading :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm [OpenCypher.ReadingClause] -> Phantoms.TTerm OpenCypher.SinglePartQuery
singlePartQueryWithReading original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

singlePartQueryWithReturn :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm (Maybe OpenCypher.Return) -> Phantoms.TTerm OpenCypher.SinglePartQuery
singlePartQueryWithReturn original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

singlePartQueryWithUpdating :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm [OpenCypher.UpdatingClause] -> Phantoms.TTerm OpenCypher.SinglePartQuery
singlePartQueryWithUpdating original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "return"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SinglePartQuery"),
              Core.projectionField = (Core.Name "return")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

singleQueryMultiPart :: Phantoms.TTerm OpenCypher.MultiPartQuery -> Phantoms.TTerm OpenCypher.SingleQuery
singleQueryMultiPart x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SingleQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "multiPart"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

singleQuerySinglePart :: Phantoms.TTerm OpenCypher.SinglePartQuery -> Phantoms.TTerm OpenCypher.SingleQuery
singleQuerySinglePart x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SingleQuery"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "singlePart"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

skip :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Skip
skip x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Skip"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

sortItem :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm (Maybe OpenCypher.SortOrder) -> Phantoms.TTerm OpenCypher.SortItem
sortItem expression order =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm order)}]}))

sortItemExpression :: Phantoms.TTerm OpenCypher.SortItem -> Phantoms.TTerm OpenCypher.Expression
sortItemExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortItemOrder :: Phantoms.TTerm OpenCypher.SortItem -> Phantoms.TTerm (Maybe OpenCypher.SortOrder)
sortItemOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
        Core.projectionField = (Core.Name "order")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

sortItemWithExpression :: Phantoms.TTerm OpenCypher.SortItem -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.SortItem
sortItemWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
              Core.projectionField = (Core.Name "order")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

sortItemWithOrder :: Phantoms.TTerm OpenCypher.SortItem -> Phantoms.TTerm (Maybe OpenCypher.SortOrder) -> Phantoms.TTerm OpenCypher.SortItem
sortItemWithOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.SortItem"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "order"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

sortOrderAscending :: Phantoms.TTerm OpenCypher.SortOrder
sortOrderAscending =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SortOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ascending"),
        Core.fieldTerm = Core.TermUnit}}))

sortOrderDescending :: Phantoms.TTerm OpenCypher.SortOrder
sortOrderDescending =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.SortOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "descending"),
        Core.fieldTerm = Core.TermUnit}}))

standaloneCall :: Phantoms.TTerm OpenCypher.ProcedureInvocation -> Phantoms.TTerm (Maybe OpenCypher.StarOrYieldItems) -> Phantoms.TTerm OpenCypher.StandaloneCall
standaloneCall call yieldItems =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTTerm call)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTTerm yieldItems)}]}))

standaloneCallCall :: Phantoms.TTerm OpenCypher.StandaloneCall -> Phantoms.TTerm OpenCypher.ProcedureInvocation
standaloneCallCall x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
        Core.projectionField = (Core.Name "call")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

standaloneCallWithCall :: Phantoms.TTerm OpenCypher.StandaloneCall -> Phantoms.TTerm OpenCypher.ProcedureInvocation -> Phantoms.TTerm OpenCypher.StandaloneCall
standaloneCallWithCall original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
              Core.projectionField = (Core.Name "yieldItems")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

standaloneCallWithYieldItems :: Phantoms.TTerm OpenCypher.StandaloneCall -> Phantoms.TTerm (Maybe OpenCypher.StarOrYieldItems) -> Phantoms.TTerm OpenCypher.StandaloneCall
standaloneCallWithYieldItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "call"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
              Core.projectionField = (Core.Name "call")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "yieldItems"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

standaloneCallYieldItems :: Phantoms.TTerm OpenCypher.StandaloneCall -> Phantoms.TTerm (Maybe OpenCypher.StarOrYieldItems)
standaloneCallYieldItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StandaloneCall"),
        Core.projectionField = (Core.Name "yieldItems")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

starOrYieldItemsItems :: Phantoms.TTerm OpenCypher.YieldItems -> Phantoms.TTerm OpenCypher.StarOrYieldItems
starOrYieldItemsItems x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StarOrYieldItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "items"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

starOrYieldItemsStar :: Phantoms.TTerm OpenCypher.StarOrYieldItems
starOrYieldItemsStar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StarOrYieldItems"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "star"),
        Core.fieldTerm = Core.TermUnit}}))

stringListNullPredicateExpression :: Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm [OpenCypher.StringListNullPredicateRightHandSide] -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpression left right =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)}]}))

stringListNullPredicateExpressionLeft :: Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
stringListNullPredicateExpressionLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
        Core.projectionField = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringListNullPredicateExpressionRight :: Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm [OpenCypher.StringListNullPredicateRightHandSide]
stringListNullPredicateExpressionRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
        Core.projectionField = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringListNullPredicateExpressionWithLeft :: Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpressionWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
              Core.projectionField = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringListNullPredicateExpressionWithRight :: Phantoms.TTerm OpenCypher.StringListNullPredicateExpression -> Phantoms.TTerm [OpenCypher.StringListNullPredicateRightHandSide] -> Phantoms.TTerm OpenCypher.StringListNullPredicateExpression
stringListNullPredicateExpressionWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateExpression"),
              Core.projectionField = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringListNullPredicateRightHandSideList :: Phantoms.TTerm OpenCypher.ListPredicateExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideList x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "list"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringListNullPredicateRightHandSideNull :: Phantoms.TTerm OpenCypher.NullPredicateExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideNull x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "null"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringListNullPredicateRightHandSideString :: Phantoms.TTerm OpenCypher.StringPredicateExpression -> Phantoms.TTerm OpenCypher.StringListNullPredicateRightHandSide
stringListNullPredicateRightHandSideString x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringListNullPredicateRightHandSide"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

stringLiteral :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.StringLiteral
stringLiteral x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.StringLiteral"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

stringPredicateExpression :: Phantoms.TTerm OpenCypher.StringPredicateOperator -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.StringPredicateExpression
stringPredicateExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

stringPredicateExpressionExpression :: Phantoms.TTerm OpenCypher.StringPredicateExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
stringPredicateExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringPredicateExpressionOperator :: Phantoms.TTerm OpenCypher.StringPredicateExpression -> Phantoms.TTerm OpenCypher.StringPredicateOperator
stringPredicateExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

stringPredicateExpressionWithExpression :: Phantoms.TTerm OpenCypher.StringPredicateExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression -> Phantoms.TTerm OpenCypher.StringPredicateExpression
stringPredicateExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

stringPredicateExpressionWithOperator :: Phantoms.TTerm OpenCypher.StringPredicateExpression -> Phantoms.TTerm OpenCypher.StringPredicateOperator -> Phantoms.TTerm OpenCypher.StringPredicateExpression
stringPredicateExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

stringPredicateOperatorContains :: Phantoms.TTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorContains =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "contains"),
        Core.fieldTerm = Core.TermUnit}}))

stringPredicateOperatorEndsWith :: Phantoms.TTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorEndsWith =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "endsWith"),
        Core.fieldTerm = Core.TermUnit}}))

stringPredicateOperatorStartsWith :: Phantoms.TTerm OpenCypher.StringPredicateOperator
stringPredicateOperatorStartsWith =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.StringPredicateOperator"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "startsWith"),
        Core.fieldTerm = Core.TermUnit}}))

unAndExpression :: Phantoms.TTerm OpenCypher.AndExpression -> Phantoms.TTerm [OpenCypher.NotExpression]
unAndExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.AndExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnonymousPatternPart :: Phantoms.TTerm OpenCypher.AnonymousPatternPart -> Phantoms.TTerm OpenCypher.PatternElement
unAnonymousPatternPart x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.AnonymousPatternPart")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unCreate :: Phantoms.TTerm OpenCypher.Create -> Phantoms.TTerm OpenCypher.Pattern
unCreate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Create")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unExpression :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.OrExpression
unExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Expression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unImplicitProcedureInvocation :: Phantoms.TTerm OpenCypher.ImplicitProcedureInvocation -> Phantoms.TTerm OpenCypher.QualifiedName
unImplicitProcedureInvocation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ImplicitProcedureInvocation")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unLimit :: Phantoms.TTerm OpenCypher.Limit -> Phantoms.TTerm OpenCypher.Expression
unLimit x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Limit")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unListLiteral :: Phantoms.TTerm OpenCypher.ListLiteral -> Phantoms.TTerm [OpenCypher.Expression]
unListLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ListLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unListPredicateExpression :: Phantoms.TTerm OpenCypher.ListPredicateExpression -> Phantoms.TTerm OpenCypher.AddOrSubtractExpression
unListPredicateExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ListPredicateExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unMapLiteral :: Phantoms.TTerm OpenCypher.MapLiteral -> Phantoms.TTerm [OpenCypher.KeyValuePair]
unMapLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.MapLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNodeLabel :: Phantoms.TTerm OpenCypher.NodeLabel -> Phantoms.TTerm String
unNodeLabel x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NodeLabel")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNodeLabels :: Phantoms.TTerm OpenCypher.NodeLabels -> Phantoms.TTerm [OpenCypher.NodeLabel]
unNodeLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NodeLabels")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNullPredicateExpression :: Phantoms.TTerm OpenCypher.NullPredicateExpression -> Phantoms.TTerm Bool
unNullPredicateExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.NullPredicateExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOrExpression :: Phantoms.TTerm OpenCypher.OrExpression -> Phantoms.TTerm [OpenCypher.XorExpression]
unOrExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.OrExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unOrder :: Phantoms.TTerm OpenCypher.Order -> Phantoms.TTerm [OpenCypher.SortItem]
unOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Order")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unParenthesizedExpression :: Phantoms.TTerm OpenCypher.ParenthesizedExpression -> Phantoms.TTerm OpenCypher.Expression
unParenthesizedExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ParenthesizedExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPattern :: Phantoms.TTerm OpenCypher.Pattern -> Phantoms.TTerm [OpenCypher.PatternPart]
unPattern x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Pattern")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPatternPredicate :: Phantoms.TTerm OpenCypher.PatternPredicate -> Phantoms.TTerm OpenCypher.RelationshipsPattern
unPatternPredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PatternPredicate")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPowerOfExpression :: Phantoms.TTerm OpenCypher.PowerOfExpression -> Phantoms.TTerm [OpenCypher.UnaryAddOrSubtractExpression]
unPowerOfExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PowerOfExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unProcedureResultField :: Phantoms.TTerm OpenCypher.ProcedureResultField -> Phantoms.TTerm String
unProcedureResultField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.ProcedureResultField")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPropertyKeyName :: Phantoms.TTerm OpenCypher.PropertyKeyName -> Phantoms.TTerm String
unPropertyKeyName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PropertyKeyName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPropertyLookup :: Phantoms.TTerm OpenCypher.PropertyLookup -> Phantoms.TTerm OpenCypher.PropertyKeyName
unPropertyLookup x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.PropertyLookup")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRelTypeName :: Phantoms.TTerm OpenCypher.RelTypeName -> Phantoms.TTerm String
unRelTypeName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.RelTypeName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRelationshipTypes :: Phantoms.TTerm OpenCypher.RelationshipTypes -> Phantoms.TTerm [OpenCypher.RelTypeName]
unRelationshipTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.RelationshipTypes")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unRemove :: Phantoms.TTerm OpenCypher.Remove -> Phantoms.TTerm [OpenCypher.RemoveItem]
unRemove x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Remove")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unReturn :: Phantoms.TTerm OpenCypher.Return -> Phantoms.TTerm OpenCypher.ProjectionBody
unReturn x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Return")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSet :: Phantoms.TTerm OpenCypher.Set -> Phantoms.TTerm [OpenCypher.SetItem]
unSet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Set")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unSkip :: Phantoms.TTerm OpenCypher.Skip -> Phantoms.TTerm OpenCypher.Expression
unSkip x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Skip")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unStringLiteral :: Phantoms.TTerm OpenCypher.StringLiteral -> Phantoms.TTerm String
unStringLiteral x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.StringLiteral")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unVariable :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm String
unVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Variable")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unWhere :: Phantoms.TTerm OpenCypher.Where -> Phantoms.TTerm OpenCypher.Expression
unWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.Where")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unXorExpression :: Phantoms.TTerm OpenCypher.XorExpression -> Phantoms.TTerm [OpenCypher.AndExpression]
unXorExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.cypher.openCypher.XorExpression")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryAddOrSubtractExpression :: Phantoms.TTerm (Maybe OpenCypher.AddOrSubtractOperator) -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpression operator expression =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm operator)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)}]}))

unaryAddOrSubtractExpressionExpression :: Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression
unaryAddOrSubtractExpressionExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryAddOrSubtractExpressionOperator :: Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TTerm (Maybe OpenCypher.AddOrSubtractOperator)
unaryAddOrSubtractExpressionOperator x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
        Core.projectionField = (Core.Name "operator")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unaryAddOrSubtractExpressionWithExpression :: Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TTerm OpenCypher.NonArithmeticOperatorExpression -> Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpressionWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
              Core.projectionField = (Core.Name "operator")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unaryAddOrSubtractExpressionWithOperator :: Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression -> Phantoms.TTerm (Maybe OpenCypher.AddOrSubtractOperator) -> Phantoms.TTerm OpenCypher.UnaryAddOrSubtractExpression
unaryAddOrSubtractExpressionWithOperator original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "operator"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.UnaryAddOrSubtractExpression"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

union :: Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.SingleQuery -> Phantoms.TTerm OpenCypher.Union
union all query =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTTerm all)},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Phantoms.unTTerm query)}]}))

unionAll :: Phantoms.TTerm OpenCypher.Union -> Phantoms.TTerm Bool
unionAll x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
        Core.projectionField = (Core.Name "all")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionQuery :: Phantoms.TTerm OpenCypher.Union -> Phantoms.TTerm OpenCypher.SingleQuery
unionQuery x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
        Core.projectionField = (Core.Name "query")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unionWithAll :: Phantoms.TTerm OpenCypher.Union -> Phantoms.TTerm Bool -> Phantoms.TTerm OpenCypher.Union
unionWithAll original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
              Core.projectionField = (Core.Name "query")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unionWithQuery :: Phantoms.TTerm OpenCypher.Union -> Phantoms.TTerm OpenCypher.SingleQuery -> Phantoms.TTerm OpenCypher.Union
unionWithQuery original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "all"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Union"),
              Core.projectionField = (Core.Name "all")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "query"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unwind :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Unwind
unwind expression variable =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm expression)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)}]}))

unwindExpression :: Phantoms.TTerm OpenCypher.Unwind -> Phantoms.TTerm OpenCypher.Expression
unwindExpression x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
        Core.projectionField = (Core.Name "expression")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unwindVariable :: Phantoms.TTerm OpenCypher.Unwind -> Phantoms.TTerm OpenCypher.Variable
unwindVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unwindWithExpression :: Phantoms.TTerm OpenCypher.Unwind -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Unwind
unwindWithExpression original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unwindWithVariable :: Phantoms.TTerm OpenCypher.Unwind -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Unwind
unwindWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "expression"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.Unwind"),
              Core.projectionField = (Core.Name "expression")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

updatingClauseCreate :: Phantoms.TTerm OpenCypher.Create -> Phantoms.TTerm OpenCypher.UpdatingClause
updatingClauseCreate x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "create"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

updatingClauseDelete :: Phantoms.TTerm OpenCypher.Delete -> Phantoms.TTerm OpenCypher.UpdatingClause
updatingClauseDelete x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delete"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

updatingClauseMerge :: Phantoms.TTerm OpenCypher.Merge -> Phantoms.TTerm OpenCypher.UpdatingClause
updatingClauseMerge x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "merge"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

updatingClauseRemove :: Phantoms.TTerm OpenCypher.Remove -> Phantoms.TTerm OpenCypher.UpdatingClause
updatingClauseRemove x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "remove"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

updatingClauseSet :: Phantoms.TTerm OpenCypher.Set -> Phantoms.TTerm OpenCypher.UpdatingClause
updatingClauseSet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.cypher.openCypher.UpdatingClause"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "set"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

variable :: Phantoms.TTerm String -> Phantoms.TTerm OpenCypher.Variable
variable x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Variable"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

variableAndNodeLabels :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.NodeLabels -> Phantoms.TTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabels variable labels =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm labels)}]}))

variableAndNodeLabelsLabels :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.NodeLabels
variableAndNodeLabelsLabels x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
        Core.projectionField = (Core.Name "labels")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableAndNodeLabelsVariable :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.Variable
variableAndNodeLabelsVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableAndNodeLabelsWithLabels :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.NodeLabels -> Phantoms.TTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabelsWithLabels original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableAndNodeLabelsWithVariable :: Phantoms.TTerm OpenCypher.VariableAndNodeLabels -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.VariableAndNodeLabels
variableAndNodeLabelsWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "labels"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableAndNodeLabels"),
              Core.projectionField = (Core.Name "labels")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableEquals :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.VariableEquals
variableEquals lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

variableEqualsLhs :: Phantoms.TTerm OpenCypher.VariableEquals -> Phantoms.TTerm OpenCypher.Variable
variableEqualsLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableEqualsRhs :: Phantoms.TTerm OpenCypher.VariableEquals -> Phantoms.TTerm OpenCypher.Expression
variableEqualsRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableEqualsWithLhs :: Phantoms.TTerm OpenCypher.VariableEquals -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.VariableEquals
variableEqualsWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableEqualsWithRhs :: Phantoms.TTerm OpenCypher.VariableEquals -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.VariableEquals
variableEqualsWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariableEquals"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variablePlusEquals :: Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.VariablePlusEquals
variablePlusEquals lhs rhs =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm lhs)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm rhs)}]}))

variablePlusEqualsLhs :: Phantoms.TTerm OpenCypher.VariablePlusEquals -> Phantoms.TTerm OpenCypher.Variable
variablePlusEqualsLhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
        Core.projectionField = (Core.Name "lhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablePlusEqualsRhs :: Phantoms.TTerm OpenCypher.VariablePlusEquals -> Phantoms.TTerm OpenCypher.Expression
variablePlusEqualsRhs x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
        Core.projectionField = (Core.Name "rhs")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variablePlusEqualsWithLhs :: Phantoms.TTerm OpenCypher.VariablePlusEquals -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.VariablePlusEquals
variablePlusEqualsWithLhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
              Core.projectionField = (Core.Name "rhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variablePlusEqualsWithRhs :: Phantoms.TTerm OpenCypher.VariablePlusEquals -> Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.VariablePlusEquals
variablePlusEqualsWithRhs original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "lhs"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.VariablePlusEquals"),
              Core.projectionField = (Core.Name "lhs")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rhs"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

where_ :: Phantoms.TTerm OpenCypher.Expression -> Phantoms.TTerm OpenCypher.Where
where_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.Where"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

with :: Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.With
with projection where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm projection)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)}]}))

withClause :: Phantoms.TTerm [OpenCypher.ReadingClause] -> Phantoms.TTerm [OpenCypher.UpdatingClause] -> Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm OpenCypher.WithClause
withClause reading updating with =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTTerm reading)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTTerm updating)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm with)}]}))

withClauseReading :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm [OpenCypher.ReadingClause]
withClauseReading x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionField = (Core.Name "reading")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withClauseUpdating :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm [OpenCypher.UpdatingClause]
withClauseUpdating x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionField = (Core.Name "updating")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withClauseWith :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm OpenCypher.With
withClauseWith x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
        Core.projectionField = (Core.Name "with")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withClauseWithReading :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm [OpenCypher.ReadingClause] -> Phantoms.TTerm OpenCypher.WithClause
withClauseWithReading original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

withClauseWithUpdating :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm [OpenCypher.UpdatingClause] -> Phantoms.TTerm OpenCypher.WithClause
withClauseWithUpdating original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "with")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

withClauseWithWith :: Phantoms.TTerm OpenCypher.WithClause -> Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm OpenCypher.WithClause
withClauseWithWith original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "reading"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "reading")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "updating"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.WithClause"),
              Core.projectionField = (Core.Name "updating")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "with"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

withProjection :: Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm OpenCypher.ProjectionBody
withProjection x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
        Core.projectionField = (Core.Name "projection")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withWhere :: Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm (Maybe OpenCypher.Where)
withWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

withWithProjection :: Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm OpenCypher.ProjectionBody -> Phantoms.TTerm OpenCypher.With
withWithProjection original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

withWithWhere :: Phantoms.TTerm OpenCypher.With -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.With
withWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.With"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "projection"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.With"),
              Core.projectionField = (Core.Name "projection")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

xorExpression :: Phantoms.TTerm [OpenCypher.AndExpression] -> Phantoms.TTerm OpenCypher.XorExpression
xorExpression x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.cypher.openCypher.XorExpression"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

yieldItem :: Phantoms.TTerm (Maybe OpenCypher.ProcedureResultField) -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.YieldItem
yieldItem field variable =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)}]}))

yieldItemField :: Phantoms.TTerm OpenCypher.YieldItem -> Phantoms.TTerm (Maybe OpenCypher.ProcedureResultField)
yieldItemField x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
        Core.projectionField = (Core.Name "field")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

yieldItemVariable :: Phantoms.TTerm OpenCypher.YieldItem -> Phantoms.TTerm OpenCypher.Variable
yieldItemVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
        Core.projectionField = (Core.Name "variable")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

yieldItemWithField :: Phantoms.TTerm OpenCypher.YieldItem -> Phantoms.TTerm (Maybe OpenCypher.ProcedureResultField) -> Phantoms.TTerm OpenCypher.YieldItem
yieldItemWithField original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
              Core.projectionField = (Core.Name "variable")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

yieldItemWithVariable :: Phantoms.TTerm OpenCypher.YieldItem -> Phantoms.TTerm OpenCypher.Variable -> Phantoms.TTerm OpenCypher.YieldItem
yieldItemWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItem"),
              Core.projectionField = (Core.Name "field")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

yieldItems :: Phantoms.TTerm [OpenCypher.YieldItem] -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.YieldItems
yieldItems items where_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm items)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm where_)}]}))

yieldItemsItems :: Phantoms.TTerm OpenCypher.YieldItems -> Phantoms.TTerm [OpenCypher.YieldItem]
yieldItemsItems x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
        Core.projectionField = (Core.Name "items")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

yieldItemsWhere :: Phantoms.TTerm OpenCypher.YieldItems -> Phantoms.TTerm (Maybe OpenCypher.Where)
yieldItemsWhere x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
        Core.projectionField = (Core.Name "where")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

yieldItemsWithItems :: Phantoms.TTerm OpenCypher.YieldItems -> Phantoms.TTerm [OpenCypher.YieldItem] -> Phantoms.TTerm OpenCypher.YieldItems
yieldItemsWithItems original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
              Core.projectionField = (Core.Name "where")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

yieldItemsWithWhere :: Phantoms.TTerm OpenCypher.YieldItems -> Phantoms.TTerm (Maybe OpenCypher.Where) -> Phantoms.TTerm OpenCypher.YieldItems
yieldItemsWithWhere original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "items"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.cypher.openCypher.YieldItems"),
              Core.projectionField = (Core.Name "items")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "where"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
