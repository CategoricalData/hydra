-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.testing

module Hydra.Dsl.Testing where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Core as Core_
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Module as Module
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Testing as Testing
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

alphaConversionTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.AlphaConversionTestCase
alphaConversionTestCase term oldVariable newVariable result =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "oldVariable"),
          Core.fieldTerm = (Phantoms.unTTerm oldVariable)},
        Core.Field {
          Core.fieldName = (Core.Name "newVariable"),
          Core.fieldTerm = (Phantoms.unTTerm newVariable)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm result)}]}))

alphaConversionTestCaseTerm :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Term
alphaConversionTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alphaConversionTestCaseOldVariable :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Name
alphaConversionTestCaseOldVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
        Core.projectionField = (Core.Name "oldVariable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alphaConversionTestCaseNewVariable :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Name
alphaConversionTestCaseNewVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
        Core.projectionField = (Core.Name "newVariable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alphaConversionTestCaseResult :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Term
alphaConversionTestCaseResult x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
        Core.projectionField = (Core.Name "result")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

alphaConversionTestCaseWithTerm :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.AlphaConversionTestCase
alphaConversionTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "oldVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "oldVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "newVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alphaConversionTestCaseWithOldVariable :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Testing.AlphaConversionTestCase
alphaConversionTestCaseWithOldVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "oldVariable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "newVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "newVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alphaConversionTestCaseWithNewVariable :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Testing.AlphaConversionTestCase
alphaConversionTestCaseWithNewVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "oldVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "oldVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newVariable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "result")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

alphaConversionTestCaseWithResult :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.AlphaConversionTestCase
alphaConversionTestCaseWithResult original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "oldVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "oldVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "newVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.AlphaConversionTestCase"),
              Core.projectionField = (Core.Name "newVariable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "result"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

evaluationStyleEager :: Phantoms.TTerm Testing.EvaluationStyle
evaluationStyleEager =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.EvaluationStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "eager"),
        Core.fieldTerm = Core.TermUnit}}))

evaluationStyleLazy :: Phantoms.TTerm Testing.EvaluationStyle
evaluationStyleLazy =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.EvaluationStyle"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lazy"),
        Core.fieldTerm = Core.TermUnit}}))

caseConversionTestCase :: Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm Testing.CaseConversionTestCase
caseConversionTestCase fromConvention toConvention fromString toString =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fromConvention"),
          Core.fieldTerm = (Phantoms.unTTerm fromConvention)},
        Core.Field {
          Core.fieldName = (Core.Name "toConvention"),
          Core.fieldTerm = (Phantoms.unTTerm toConvention)},
        Core.Field {
          Core.fieldName = (Core.Name "fromString"),
          Core.fieldTerm = (Phantoms.unTTerm fromString)},
        Core.Field {
          Core.fieldName = (Core.Name "toString"),
          Core.fieldTerm = (Phantoms.unTTerm toString)}]}))

caseConversionTestCaseFromConvention :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm Util.CaseConvention
caseConversionTestCaseFromConvention x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
        Core.projectionField = (Core.Name "fromConvention")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseConversionTestCaseToConvention :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm Util.CaseConvention
caseConversionTestCaseToConvention x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
        Core.projectionField = (Core.Name "toConvention")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseConversionTestCaseFromString :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm String
caseConversionTestCaseFromString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
        Core.projectionField = (Core.Name "fromString")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseConversionTestCaseToString :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm String
caseConversionTestCaseToString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
        Core.projectionField = (Core.Name "toString")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

caseConversionTestCaseWithFromConvention :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm Testing.CaseConversionTestCase
caseConversionTestCaseWithFromConvention original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fromConvention"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "toConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fromString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseConversionTestCaseWithToConvention :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm Util.CaseConvention -> Phantoms.TTerm Testing.CaseConversionTestCase
caseConversionTestCaseWithToConvention original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fromConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toConvention"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fromString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseConversionTestCaseWithFromString :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.CaseConversionTestCase
caseConversionTestCaseWithFromString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fromConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fromString"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "toString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

caseConversionTestCaseWithToString :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.CaseConversionTestCase
caseConversionTestCaseWithToString original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "fromConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toConvention"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "toConvention")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fromString"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.CaseConversionTestCase"),
              Core.projectionField = (Core.Name "fromString")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "toString"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

delegatedEvaluationTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DelegatedEvaluationTestCase
delegatedEvaluationTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

delegatedEvaluationTestCaseInput :: Phantoms.TTerm Testing.DelegatedEvaluationTestCase -> Phantoms.TTerm Core.Term
delegatedEvaluationTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

delegatedEvaluationTestCaseOutput :: Phantoms.TTerm Testing.DelegatedEvaluationTestCase -> Phantoms.TTerm Core.Term
delegatedEvaluationTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

delegatedEvaluationTestCaseWithInput :: Phantoms.TTerm Testing.DelegatedEvaluationTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DelegatedEvaluationTestCase
delegatedEvaluationTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

delegatedEvaluationTestCaseWithOutput :: Phantoms.TTerm Testing.DelegatedEvaluationTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DelegatedEvaluationTestCase
delegatedEvaluationTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DelegatedEvaluationTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

etaExpansionTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EtaExpansionTestCase
etaExpansionTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

etaExpansionTestCaseInput :: Phantoms.TTerm Testing.EtaExpansionTestCase -> Phantoms.TTerm Core.Term
etaExpansionTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

etaExpansionTestCaseOutput :: Phantoms.TTerm Testing.EtaExpansionTestCase -> Phantoms.TTerm Core.Term
etaExpansionTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

etaExpansionTestCaseWithInput :: Phantoms.TTerm Testing.EtaExpansionTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EtaExpansionTestCase
etaExpansionTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

etaExpansionTestCaseWithOutput :: Phantoms.TTerm Testing.EtaExpansionTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EtaExpansionTestCase
etaExpansionTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EtaExpansionTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

deannotateTermTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DeannotateTermTestCase
deannotateTermTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

deannotateTermTestCaseInput :: Phantoms.TTerm Testing.DeannotateTermTestCase -> Phantoms.TTerm Core.Term
deannotateTermTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deannotateTermTestCaseOutput :: Phantoms.TTerm Testing.DeannotateTermTestCase -> Phantoms.TTerm Core.Term
deannotateTermTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deannotateTermTestCaseWithInput :: Phantoms.TTerm Testing.DeannotateTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DeannotateTermTestCase
deannotateTermTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

deannotateTermTestCaseWithOutput :: Phantoms.TTerm Testing.DeannotateTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.DeannotateTermTestCase
deannotateTermTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

deannotateTypeTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.DeannotateTypeTestCase
deannotateTypeTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

deannotateTypeTestCaseInput :: Phantoms.TTerm Testing.DeannotateTypeTestCase -> Phantoms.TTerm Core.Type
deannotateTypeTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deannotateTypeTestCaseOutput :: Phantoms.TTerm Testing.DeannotateTypeTestCase -> Phantoms.TTerm Core.Type
deannotateTypeTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

deannotateTypeTestCaseWithInput :: Phantoms.TTerm Testing.DeannotateTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.DeannotateTypeTestCase
deannotateTypeTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

deannotateTypeTestCaseWithOutput :: Phantoms.TTerm Testing.DeannotateTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.DeannotateTypeTestCase
deannotateTypeTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.DeannotateTypeTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

flattenLetTermsTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FlattenLetTermsTestCase
flattenLetTermsTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

flattenLetTermsTestCaseInput :: Phantoms.TTerm Testing.FlattenLetTermsTestCase -> Phantoms.TTerm Core.Term
flattenLetTermsTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

flattenLetTermsTestCaseOutput :: Phantoms.TTerm Testing.FlattenLetTermsTestCase -> Phantoms.TTerm Core.Term
flattenLetTermsTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

flattenLetTermsTestCaseWithInput :: Phantoms.TTerm Testing.FlattenLetTermsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FlattenLetTermsTestCase
flattenLetTermsTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

flattenLetTermsTestCaseWithOutput :: Phantoms.TTerm Testing.FlattenLetTermsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FlattenLetTermsTestCase
flattenLetTermsTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FlattenLetTermsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

foldOperationSumInt32Literals :: Phantoms.TTerm Testing.FoldOperation
foldOperationSumInt32Literals =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sumInt32Literals"),
        Core.fieldTerm = Core.TermUnit}}))

foldOperationCollectListLengths :: Phantoms.TTerm Testing.FoldOperation
foldOperationCollectListLengths =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collectListLengths"),
        Core.fieldTerm = Core.TermUnit}}))

foldOperationCollectLabels :: Phantoms.TTerm Testing.FoldOperation
foldOperationCollectLabels =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.FoldOperation"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "collectLabels"),
        Core.fieldTerm = Core.TermUnit}}))

foldOverTermTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Coders.TraversalOrder -> Phantoms.TTerm Testing.FoldOperation -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FoldOverTermTestCase
foldOverTermTestCase input traversalOrder operation output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "traversalOrder"),
          Core.fieldTerm = (Phantoms.unTTerm traversalOrder)},
        Core.Field {
          Core.fieldName = (Core.Name "operation"),
          Core.fieldTerm = (Phantoms.unTTerm operation)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

foldOverTermTestCaseInput :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Core.Term
foldOverTermTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foldOverTermTestCaseTraversalOrder :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Coders.TraversalOrder
foldOverTermTestCaseTraversalOrder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
        Core.projectionField = (Core.Name "traversalOrder")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foldOverTermTestCaseOperation :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Testing.FoldOperation
foldOverTermTestCaseOperation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
        Core.projectionField = (Core.Name "operation")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foldOverTermTestCaseOutput :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Core.Term
foldOverTermTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

foldOverTermTestCaseWithInput :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FoldOverTermTestCase
foldOverTermTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "traversalOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "traversalOrder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "operation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

foldOverTermTestCaseWithTraversalOrder :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Coders.TraversalOrder -> Phantoms.TTerm Testing.FoldOverTermTestCase
foldOverTermTestCaseWithTraversalOrder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversalOrder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "operation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "operation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

foldOverTermTestCaseWithOperation :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Testing.FoldOperation -> Phantoms.TTerm Testing.FoldOverTermTestCase
foldOverTermTestCaseWithOperation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversalOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "traversalOrder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

foldOverTermTestCaseWithOutput :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FoldOverTermTestCase
foldOverTermTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "traversalOrder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "traversalOrder")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "operation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FoldOverTermTestCase"),
              Core.projectionField = (Core.Name "operation")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

freeVariablesTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Testing.FreeVariablesTestCase
freeVariablesTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

freeVariablesTestCaseInput :: Phantoms.TTerm Testing.FreeVariablesTestCase -> Phantoms.TTerm Core.Term
freeVariablesTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

freeVariablesTestCaseOutput :: Phantoms.TTerm Testing.FreeVariablesTestCase -> Phantoms.TTerm (S.Set Core.Name)
freeVariablesTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

freeVariablesTestCaseWithInput :: Phantoms.TTerm Testing.FreeVariablesTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.FreeVariablesTestCase
freeVariablesTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

freeVariablesTestCaseWithOutput :: Phantoms.TTerm Testing.FreeVariablesTestCase -> Phantoms.TTerm (S.Set Core.Name) -> Phantoms.TTerm Testing.FreeVariablesTestCase
freeVariablesTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.FreeVariablesTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hoistPredicateCaseStatements :: Phantoms.TTerm Testing.HoistPredicate
hoistPredicateCaseStatements =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.HoistPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseStatements"),
        Core.fieldTerm = Core.TermUnit}}))

hoistPredicateApplications :: Phantoms.TTerm Testing.HoistPredicate
hoistPredicateApplications =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.HoistPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "applications"),
        Core.fieldTerm = Core.TermUnit}}))

hoistPredicateLists :: Phantoms.TTerm Testing.HoistPredicate
hoistPredicateLists =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.HoistPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "lists"),
        Core.fieldTerm = Core.TermUnit}}))

hoistPredicateNothing :: Phantoms.TTerm Testing.HoistPredicate
hoistPredicateNothing =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.HoistPredicate"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nothing"),
        Core.fieldTerm = Core.TermUnit}}))

hoistLetBindingsTestCase :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistLetBindingsTestCase
hoistLetBindingsTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

hoistLetBindingsTestCaseInput :: Phantoms.TTerm Testing.HoistLetBindingsTestCase -> Phantoms.TTerm Core.Let
hoistLetBindingsTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistLetBindingsTestCaseOutput :: Phantoms.TTerm Testing.HoistLetBindingsTestCase -> Phantoms.TTerm Core.Let
hoistLetBindingsTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistLetBindingsTestCaseWithInput :: Phantoms.TTerm Testing.HoistLetBindingsTestCase -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistLetBindingsTestCase
hoistLetBindingsTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hoistLetBindingsTestCaseWithOutput :: Phantoms.TTerm Testing.HoistLetBindingsTestCase -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistLetBindingsTestCase
hoistLetBindingsTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistLetBindingsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hoistPolymorphicLetBindingsTestCase :: Phantoms.TTerm Core.Let -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase
hoistPolymorphicLetBindingsTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

hoistPolymorphicLetBindingsTestCaseInput :: Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase -> Phantoms.TTerm Core.Let
hoistPolymorphicLetBindingsTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistPolymorphicLetBindingsTestCaseOutput :: Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase -> Phantoms.TTerm Core.Let
hoistPolymorphicLetBindingsTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistPolymorphicLetBindingsTestCaseWithInput :: Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase
hoistPolymorphicLetBindingsTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hoistPolymorphicLetBindingsTestCaseWithOutput :: Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase -> Phantoms.TTerm Core.Let -> Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase
hoistPolymorphicLetBindingsTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistPolymorphicLetBindingsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hoistSubtermsTestCase :: Phantoms.TTerm Testing.HoistPredicate -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistSubtermsTestCase
hoistSubtermsTestCase predicate input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm predicate)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

hoistSubtermsTestCasePredicate :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Testing.HoistPredicate
hoistSubtermsTestCasePredicate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
        Core.projectionField = (Core.Name "predicate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistSubtermsTestCaseInput :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Core.Term
hoistSubtermsTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistSubtermsTestCaseOutput :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Core.Term
hoistSubtermsTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistSubtermsTestCaseWithPredicate :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Testing.HoistPredicate -> Phantoms.TTerm Testing.HoistSubtermsTestCase
hoistSubtermsTestCaseWithPredicate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hoistSubtermsTestCaseWithInput :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistSubtermsTestCase
hoistSubtermsTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hoistSubtermsTestCaseWithOutput :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistSubtermsTestCase
hoistSubtermsTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "predicate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "predicate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistSubtermsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

hoistCaseStatementsTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistCaseStatementsTestCase
hoistCaseStatementsTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

hoistCaseStatementsTestCaseInput :: Phantoms.TTerm Testing.HoistCaseStatementsTestCase -> Phantoms.TTerm Core.Term
hoistCaseStatementsTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistCaseStatementsTestCaseOutput :: Phantoms.TTerm Testing.HoistCaseStatementsTestCase -> Phantoms.TTerm Core.Term
hoistCaseStatementsTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

hoistCaseStatementsTestCaseWithInput :: Phantoms.TTerm Testing.HoistCaseStatementsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistCaseStatementsTestCase
hoistCaseStatementsTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

hoistCaseStatementsTestCaseWithOutput :: Phantoms.TTerm Testing.HoistCaseStatementsTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.HoistCaseStatementsTestCase
hoistCaseStatementsTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.HoistCaseStatementsTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

termRewriterReplaceFooWithBar :: Phantoms.TTerm Testing.TermRewriter
termRewriterReplaceFooWithBar =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TermRewriter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "replaceFooWithBar"),
        Core.fieldTerm = Core.TermUnit}}))

termRewriterReplaceInt32WithInt64 :: Phantoms.TTerm Testing.TermRewriter
termRewriterReplaceInt32WithInt64 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TermRewriter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "replaceInt32WithInt64"),
        Core.fieldTerm = Core.TermUnit}}))

rewriteTermTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TermRewriter -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.RewriteTermTestCase
rewriteTermTestCase input rewriter output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Phantoms.unTTerm rewriter)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

rewriteTermTestCaseInput :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Core.Term
rewriteTermTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTermTestCaseRewriter :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Testing.TermRewriter
rewriteTermTestCaseRewriter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
        Core.projectionField = (Core.Name "rewriter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTermTestCaseOutput :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Core.Term
rewriteTermTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTermTestCaseWithInput :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.RewriteTermTestCase
rewriteTermTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "rewriter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rewriteTermTestCaseWithRewriter :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Testing.TermRewriter -> Phantoms.TTerm Testing.RewriteTermTestCase
rewriteTermTestCaseWithRewriter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rewriteTermTestCaseWithOutput :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.RewriteTermTestCase
rewriteTermTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTermTestCase"),
              Core.projectionField = (Core.Name "rewriter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeRewriterReplaceStringWithInt32 :: Phantoms.TTerm Testing.TypeRewriter
typeRewriterReplaceStringWithInt32 =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TypeRewriter"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "replaceStringWithInt32"),
        Core.fieldTerm = Core.TermUnit}}))

rewriteTypeTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeRewriter -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.RewriteTypeTestCase
rewriteTypeTestCase input rewriter output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Phantoms.unTTerm rewriter)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

rewriteTypeTestCaseInput :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Core.Type
rewriteTypeTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTypeTestCaseRewriter :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Testing.TypeRewriter
rewriteTypeTestCaseRewriter x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
        Core.projectionField = (Core.Name "rewriter")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTypeTestCaseOutput :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Core.Type
rewriteTypeTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

rewriteTypeTestCaseWithInput :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.RewriteTypeTestCase
rewriteTypeTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "rewriter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rewriteTypeTestCaseWithRewriter :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Testing.TypeRewriter -> Phantoms.TTerm Testing.RewriteTypeTestCase
rewriteTypeTestCaseWithRewriter original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

rewriteTypeTestCaseWithOutput :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.RewriteTypeTestCase
rewriteTypeTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "rewriter"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.RewriteTypeTestCase"),
              Core.projectionField = (Core.Name "rewriter")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

evaluationTestCase :: Phantoms.TTerm Testing.EvaluationStyle -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EvaluationTestCase
evaluationTestCase evaluationStyle input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "evaluationStyle"),
          Core.fieldTerm = (Phantoms.unTTerm evaluationStyle)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

evaluationTestCaseEvaluationStyle :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Testing.EvaluationStyle
evaluationTestCaseEvaluationStyle x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
        Core.projectionField = (Core.Name "evaluationStyle")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

evaluationTestCaseInput :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Core.Term
evaluationTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

evaluationTestCaseOutput :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Core.Term
evaluationTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

evaluationTestCaseWithEvaluationStyle :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Testing.EvaluationStyle -> Phantoms.TTerm Testing.EvaluationTestCase
evaluationTestCaseWithEvaluationStyle original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "evaluationStyle"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

evaluationTestCaseWithInput :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EvaluationTestCase
evaluationTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "evaluationStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "evaluationStyle")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

evaluationTestCaseWithOutput :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.EvaluationTestCase
evaluationTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "evaluationStyle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "evaluationStyle")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.EvaluationTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inferenceFailureTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.InferenceFailureTestCase
inferenceFailureTestCase input =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.InferenceFailureTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)}]}))

inferenceFailureTestCaseInput :: Phantoms.TTerm Testing.InferenceFailureTestCase -> Phantoms.TTerm Core.Term
inferenceFailureTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.InferenceFailureTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inferenceFailureTestCaseWithInput :: Phantoms.TTerm Testing.InferenceFailureTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.InferenceFailureTestCase
inferenceFailureTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.InferenceFailureTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

inferenceTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Testing.InferenceTestCase
inferenceTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

inferenceTestCaseInput :: Phantoms.TTerm Testing.InferenceTestCase -> Phantoms.TTerm Core.Term
inferenceTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inferenceTestCaseOutput :: Phantoms.TTerm Testing.InferenceTestCase -> Phantoms.TTerm Core.TypeScheme
inferenceTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inferenceTestCaseWithInput :: Phantoms.TTerm Testing.InferenceTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.InferenceTestCase
inferenceTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

inferenceTestCaseWithOutput :: Phantoms.TTerm Testing.InferenceTestCase -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Testing.InferenceTestCase
inferenceTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.InferenceTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

jsonDecodeTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Model.Value -> Phantoms.TTerm (Either String Core.Term) -> Phantoms.TTerm Testing.JsonDecodeTestCase
jsonDecodeTestCase type_ json expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm json)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

jsonDecodeTestCaseType :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm Core.Type
jsonDecodeTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonDecodeTestCaseJson :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm Model.Value
jsonDecodeTestCaseJson x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
        Core.projectionField = (Core.Name "json")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonDecodeTestCaseExpected :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm (Either String Core.Term)
jsonDecodeTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonDecodeTestCaseWithType :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.JsonDecodeTestCase
jsonDecodeTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "json")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

jsonDecodeTestCaseWithJson :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm Model.Value -> Phantoms.TTerm Testing.JsonDecodeTestCase
jsonDecodeTestCaseWithJson original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

jsonDecodeTestCaseWithExpected :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm (Either String Core.Term) -> Phantoms.TTerm Testing.JsonDecodeTestCase
jsonDecodeTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "json"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonDecodeTestCase"),
              Core.projectionField = (Core.Name "json")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

jsonEncodeTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm (Either String Model.Value) -> Phantoms.TTerm Testing.JsonEncodeTestCase
jsonEncodeTestCase term expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

jsonEncodeTestCaseTerm :: Phantoms.TTerm Testing.JsonEncodeTestCase -> Phantoms.TTerm Core.Term
jsonEncodeTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonEncodeTestCaseExpected :: Phantoms.TTerm Testing.JsonEncodeTestCase -> Phantoms.TTerm (Either String Model.Value)
jsonEncodeTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonEncodeTestCaseWithTerm :: Phantoms.TTerm Testing.JsonEncodeTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.JsonEncodeTestCase
jsonEncodeTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

jsonEncodeTestCaseWithExpected :: Phantoms.TTerm Testing.JsonEncodeTestCase -> Phantoms.TTerm (Either String Model.Value) -> Phantoms.TTerm Testing.JsonEncodeTestCase
jsonEncodeTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonEncodeTestCase"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

jsonRoundtripTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.JsonRoundtripTestCase
jsonRoundtripTestCase type_ term =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)}]}))

jsonRoundtripTestCaseType :: Phantoms.TTerm Testing.JsonRoundtripTestCase -> Phantoms.TTerm Core.Type
jsonRoundtripTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonRoundtripTestCaseTerm :: Phantoms.TTerm Testing.JsonRoundtripTestCase -> Phantoms.TTerm Core.Term
jsonRoundtripTestCaseTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

jsonRoundtripTestCaseWithType :: Phantoms.TTerm Testing.JsonRoundtripTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.JsonRoundtripTestCase
jsonRoundtripTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

jsonRoundtripTestCaseWithTerm :: Phantoms.TTerm Testing.JsonRoundtripTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.JsonRoundtripTestCase
jsonRoundtripTestCaseWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JsonRoundtripTestCase"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

liftLambdaAboveLetTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase
liftLambdaAboveLetTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

liftLambdaAboveLetTestCaseInput :: Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase -> Phantoms.TTerm Core.Term
liftLambdaAboveLetTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

liftLambdaAboveLetTestCaseOutput :: Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase -> Phantoms.TTerm Core.Term
liftLambdaAboveLetTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

liftLambdaAboveLetTestCaseWithInput :: Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase
liftLambdaAboveLetTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

liftLambdaAboveLetTestCaseWithOutput :: Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase
liftLambdaAboveLetTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.LiftLambdaAboveLetTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

parserTestCase :: Phantoms.TTerm String -> Phantoms.TTerm (Parsing.ParseResult a) -> Phantoms.TTerm (Testing.ParserTestCase a)
parserTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ParserTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

parserTestCaseInput :: Phantoms.TTerm (Testing.ParserTestCase a) -> Phantoms.TTerm String
parserTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.ParserTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parserTestCaseOutput :: Phantoms.TTerm (Testing.ParserTestCase a) -> Phantoms.TTerm (Parsing.ParseResult a)
parserTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.ParserTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

parserTestCaseWithInput :: Phantoms.TTerm (Testing.ParserTestCase a) -> Phantoms.TTerm String -> Phantoms.TTerm (Testing.ParserTestCase a)
parserTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ParserTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.ParserTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

parserTestCaseWithOutput :: Phantoms.TTerm (Testing.ParserTestCase a) -> Phantoms.TTerm (Parsing.ParseResult a) -> Phantoms.TTerm (Testing.ParserTestCase a)
parserTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ParserTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.ParserTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

tag :: Phantoms.TTerm String -> Phantoms.TTerm Testing.Tag
tag x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.testing.Tag"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unTag :: Phantoms.TTerm Testing.Tag -> Phantoms.TTerm String
unTag x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.testing.Tag")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodec :: Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Module.FileExtension -> Phantoms.TTerm (Core.Term -> Graph.Graph -> Either String String) -> Phantoms.TTerm (Core.Type -> Graph.Graph -> Either String String) -> Phantoms.TTerm (String -> String) -> Phantoms.TTerm (Module.Namespace -> String) -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm String -> Phantoms.TTerm (S.Set Core.Name -> [String]) -> Phantoms.TTerm Testing.TestCodec
testCodec language fileExtension encodeTerm encodeType formatTestName formatModuleName testCaseTemplate testGroupTemplate moduleTemplate importTemplate findImports =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm language)},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Phantoms.unTTerm fileExtension)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Phantoms.unTTerm encodeTerm)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Phantoms.unTTerm encodeType)},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Phantoms.unTTerm formatTestName)},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Phantoms.unTTerm formatModuleName)},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm testCaseTemplate)},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm testGroupTemplate)},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm moduleTemplate)},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm importTemplate)},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Phantoms.unTTerm findImports)}]}))

testCodecLanguage :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm Coders.LanguageName
testCodecLanguage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "language")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecFileExtension :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm Module.FileExtension
testCodecFileExtension x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "fileExtension")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecEncodeTerm :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Core.Term -> Graph.Graph -> Either String String)
testCodecEncodeTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "encodeTerm")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecEncodeType :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Core.Type -> Graph.Graph -> Either String String)
testCodecEncodeType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "encodeType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecFormatTestName :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (String -> String)
testCodecFormatTestName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "formatTestName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecFormatModuleName :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Module.Namespace -> String)
testCodecFormatModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "formatModuleName")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecTestCaseTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String
testCodecTestCaseTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "testCaseTemplate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecTestGroupTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String
testCodecTestGroupTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "testGroupTemplate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecModuleTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String
testCodecModuleTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "moduleTemplate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecImportTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String
testCodecImportTemplate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "importTemplate")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecFindImports :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (S.Set Core.Name -> [String])
testCodecFindImports x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
        Core.projectionField = (Core.Name "findImports")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCodecWithLanguage :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Testing.TestCodec
testCodecWithLanguage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithFileExtension :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm Module.FileExtension -> Phantoms.TTerm Testing.TestCodec
testCodecWithFileExtension original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithEncodeTerm :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Core.Term -> Graph.Graph -> Either String String) -> Phantoms.TTerm Testing.TestCodec
testCodecWithEncodeTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithEncodeType :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Core.Type -> Graph.Graph -> Either String String) -> Phantoms.TTerm Testing.TestCodec
testCodecWithEncodeType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithFormatTestName :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (String -> String) -> Phantoms.TTerm Testing.TestCodec
testCodecWithFormatTestName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithFormatModuleName :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (Module.Namespace -> String) -> Phantoms.TTerm Testing.TestCodec
testCodecWithFormatModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithTestCaseTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCodec
testCodecWithTestCaseTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithTestGroupTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCodec
testCodecWithTestGroupTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithModuleTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCodec
testCodecWithModuleTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithImportTemplate :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCodec
testCodecWithImportTemplate original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "findImports")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCodecWithFindImports :: Phantoms.TTerm Testing.TestCodec -> Phantoms.TTerm (S.Set Core.Name -> [String]) -> Phantoms.TTerm Testing.TestCodec
testCodecWithFindImports original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCodec"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "language")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "fileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "fileExtension")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "encodeType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "encodeType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatTestName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatTestName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "formatModuleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "formatModuleName")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testCaseTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testCaseTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "testGroupTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "testGroupTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "moduleTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "moduleTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "importTemplate"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCodec"),
              Core.projectionField = (Core.Name "importTemplate")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "findImports"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

testGenerator :: Phantoms.TTerm (Module.Module -> Graph.Graph -> Either String (Module.Namespaces a)) -> Phantoms.TTerm (Module.Namespaces a -> Testing.TestCodec) -> Phantoms.TTerm (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)) -> Phantoms.TTerm (Maybe (String -> [Module.Module] -> (String, String))) -> Phantoms.TTerm (Testing.TestGenerator a)
testGenerator namespacesForModule createCodec generateTestFile aggregatorFile =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespacesForModule"),
          Core.fieldTerm = (Phantoms.unTTerm namespacesForModule)},
        Core.Field {
          Core.fieldName = (Core.Name "createCodec"),
          Core.fieldTerm = (Phantoms.unTTerm createCodec)},
        Core.Field {
          Core.fieldName = (Core.Name "generateTestFile"),
          Core.fieldTerm = (Phantoms.unTTerm generateTestFile)},
        Core.Field {
          Core.fieldName = (Core.Name "aggregatorFile"),
          Core.fieldTerm = (Phantoms.unTTerm aggregatorFile)}]}))

testGeneratorNamespacesForModule :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Module -> Graph.Graph -> Either String (Module.Namespaces a))
testGeneratorNamespacesForModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
        Core.projectionField = (Core.Name "namespacesForModule")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGeneratorCreateCodec :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Namespaces a -> Testing.TestCodec)
testGeneratorCreateCodec x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
        Core.projectionField = (Core.Name "createCodec")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGeneratorGenerateTestFile :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String))
testGeneratorGenerateTestFile x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
        Core.projectionField = (Core.Name "generateTestFile")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGeneratorAggregatorFile :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Maybe (String -> [Module.Module] -> (String, String)))
testGeneratorAggregatorFile x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
        Core.projectionField = (Core.Name "aggregatorFile")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGeneratorWithNamespacesForModule :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Module -> Graph.Graph -> Either String (Module.Namespaces a)) -> Phantoms.TTerm (Testing.TestGenerator a)
testGeneratorWithNamespacesForModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespacesForModule"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "createCodec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "createCodec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generateTestFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "generateTestFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aggregatorFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "aggregatorFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGeneratorWithCreateCodec :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Namespaces a -> Testing.TestCodec) -> Phantoms.TTerm (Testing.TestGenerator a)
testGeneratorWithCreateCodec original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespacesForModule"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "namespacesForModule")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createCodec"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "generateTestFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "generateTestFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aggregatorFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "aggregatorFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGeneratorWithGenerateTestFile :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Module.Module -> Testing.TestGroup -> Graph.Graph -> Either String (String, String)) -> Phantoms.TTerm (Testing.TestGenerator a)
testGeneratorWithGenerateTestFile original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespacesForModule"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "namespacesForModule")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createCodec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "createCodec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generateTestFile"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "aggregatorFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "aggregatorFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGeneratorWithAggregatorFile :: Phantoms.TTerm (Testing.TestGenerator a) -> Phantoms.TTerm (Maybe (String -> [Module.Module] -> (String, String))) -> Phantoms.TTerm (Testing.TestGenerator a)
testGeneratorWithAggregatorFile original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGenerator"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespacesForModule"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "namespacesForModule")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "createCodec"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "createCodec")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "generateTestFile"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGenerator"),
              Core.projectionField = (Core.Name "generateTestFile")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "aggregatorFile"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

testCaseAlphaConversion :: Phantoms.TTerm Testing.AlphaConversionTestCase -> Phantoms.TTerm Testing.TestCase
testCaseAlphaConversion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "alphaConversion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseCaseConversion :: Phantoms.TTerm Testing.CaseConversionTestCase -> Phantoms.TTerm Testing.TestCase
testCaseCaseConversion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "caseConversion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseDeannotateTerm :: Phantoms.TTerm Testing.DeannotateTermTestCase -> Phantoms.TTerm Testing.TestCase
testCaseDeannotateTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deannotateTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseDeannotateType :: Phantoms.TTerm Testing.DeannotateTypeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseDeannotateType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "deannotateType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseDelegatedEvaluation :: Phantoms.TTerm Testing.DelegatedEvaluationTestCase -> Phantoms.TTerm Testing.TestCase
testCaseDelegatedEvaluation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "delegatedEvaluation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseEtaExpansion :: Phantoms.TTerm Testing.EtaExpansionTestCase -> Phantoms.TTerm Testing.TestCase
testCaseEtaExpansion x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "etaExpansion"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseFlattenLetTerms :: Phantoms.TTerm Testing.FlattenLetTermsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseFlattenLetTerms x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "flattenLetTerms"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseFreeVariables :: Phantoms.TTerm Testing.FreeVariablesTestCase -> Phantoms.TTerm Testing.TestCase
testCaseFreeVariables x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "freeVariables"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseEvaluation :: Phantoms.TTerm Testing.EvaluationTestCase -> Phantoms.TTerm Testing.TestCase
testCaseEvaluation x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "evaluation"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseInference :: Phantoms.TTerm Testing.InferenceTestCase -> Phantoms.TTerm Testing.TestCase
testCaseInference x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inference"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseInferenceFailure :: Phantoms.TTerm Testing.InferenceFailureTestCase -> Phantoms.TTerm Testing.TestCase
testCaseInferenceFailure x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "inferenceFailure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJsonDecode :: Phantoms.TTerm Testing.JsonDecodeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJsonDecode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jsonDecode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJsonEncode :: Phantoms.TTerm Testing.JsonEncodeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJsonEncode x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jsonEncode"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJsonParser :: Phantoms.TTerm Testing.JsonParserTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJsonParser x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jsonParser"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJsonRoundtrip :: Phantoms.TTerm Testing.JsonRoundtripTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJsonRoundtrip x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jsonRoundtrip"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJsonWriter :: Phantoms.TTerm Testing.JsonWriterTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJsonWriter x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "jsonWriter"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseLiftLambdaAboveLet :: Phantoms.TTerm Testing.LiftLambdaAboveLetTestCase -> Phantoms.TTerm Testing.TestCase
testCaseLiftLambdaAboveLet x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "liftLambdaAboveLet"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseSerialization :: Phantoms.TTerm Testing.SerializationTestCase -> Phantoms.TTerm Testing.TestCase
testCaseSerialization x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "serialization"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseSimplifyTerm :: Phantoms.TTerm Testing.SimplifyTermTestCase -> Phantoms.TTerm Testing.TestCase
testCaseSimplifyTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "simplifyTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTopologicalSort :: Phantoms.TTerm Testing.TopologicalSortTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTopologicalSort x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "topologicalSort"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTopologicalSortBindings :: Phantoms.TTerm Testing.TopologicalSortBindingsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTopologicalSortBindings x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "topologicalSortBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTopologicalSortSCC :: Phantoms.TTerm Testing.TopologicalSortSCCTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTopologicalSortSCC x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "topologicalSortSCC"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTypeChecking :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTypeChecking x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeChecking"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTypeCheckingFailure :: Phantoms.TTerm Testing.TypeCheckingFailureTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTypeCheckingFailure x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeCheckingFailure"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseTypeReduction :: Phantoms.TTerm Testing.TypeReductionTestCase -> Phantoms.TTerm Testing.TestCase
testCaseTypeReduction x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "typeReduction"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseNormalizeTypeVariables :: Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase -> Phantoms.TTerm Testing.TestCase
testCaseNormalizeTypeVariables x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalizeTypeVariables"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseFoldOverTerm :: Phantoms.TTerm Testing.FoldOverTermTestCase -> Phantoms.TTerm Testing.TestCase
testCaseFoldOverTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "foldOverTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseRewriteTerm :: Phantoms.TTerm Testing.RewriteTermTestCase -> Phantoms.TTerm Testing.TestCase
testCaseRewriteTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rewriteTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseRewriteType :: Phantoms.TTerm Testing.RewriteTypeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseRewriteType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "rewriteType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseHoistSubterms :: Phantoms.TTerm Testing.HoistSubtermsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseHoistSubterms x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistSubterms"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseHoistCaseStatements :: Phantoms.TTerm Testing.HoistCaseStatementsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseHoistCaseStatements x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistCaseStatements"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseHoistLetBindings :: Phantoms.TTerm Testing.HoistLetBindingsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseHoistLetBindings x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistLetBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseHoistPolymorphicLetBindings :: Phantoms.TTerm Testing.HoistPolymorphicLetBindingsTestCase -> Phantoms.TTerm Testing.TestCase
testCaseHoistPolymorphicLetBindings x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hoistPolymorphicLetBindings"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseSubstInType :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseSubstInType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "substInType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseVariableOccursInType :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Testing.TestCase
testCaseVariableOccursInType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "variableOccursInType"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseUnifyTypes :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm Testing.TestCase
testCaseUnifyTypes x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unifyTypes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseJoinTypes :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm Testing.TestCase
testCaseJoinTypes x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "joinTypes"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseUnshadowVariables :: Phantoms.TTerm Testing.UnshadowVariablesTestCase -> Phantoms.TTerm Testing.TestCase
testCaseUnshadowVariables x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unshadowVariables"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseValidateCoreTerm :: Phantoms.TTerm Testing.ValidateCoreTermTestCase -> Phantoms.TTerm Testing.TestCase
testCaseValidateCoreTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.testing.TestCase"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "validateCoreTerm"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

testCaseWithMetadata :: Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCase -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Testing.Tag] -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadata name case_ description tags =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm case_)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm tags)}]}))

testCaseWithMetadataName :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm String
testCaseWithMetadataName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCaseWithMetadataCase :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm Testing.TestCase
testCaseWithMetadataCase x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionField = (Core.Name "case")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCaseWithMetadataDescription :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm (Maybe String)
testCaseWithMetadataDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCaseWithMetadataTags :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm [Testing.Tag]
testCaseWithMetadataTags x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
        Core.projectionField = (Core.Name "tags")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testCaseWithMetadataWithName :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "case")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "tags")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCaseWithMetadataWithCase :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm Testing.TestCase -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithCase original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "tags")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCaseWithMetadataWithDescription :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "case")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "tags")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testCaseWithMetadataWithTags :: Phantoms.TTerm Testing.TestCaseWithMetadata -> Phantoms.TTerm [Testing.Tag] -> Phantoms.TTerm Testing.TestCaseWithMetadata
testCaseWithMetadataWithTags original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "case"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "case")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestCaseWithMetadata"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "tags"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

testGroup :: Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Testing.TestGroup] -> Phantoms.TTerm [Testing.TestCaseWithMetadata] -> Phantoms.TTerm Testing.TestGroup
testGroup name description subgroups cases =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Phantoms.unTTerm subgroups)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm cases)}]}))

testGroupName :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm String
testGroupName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGroupDescription :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm (Maybe String)
testGroupDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGroupSubgroups :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestGroup]
testGroupSubgroups x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionField = (Core.Name "subgroups")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGroupCases :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestCaseWithMetadata]
testGroupCases x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
        Core.projectionField = (Core.Name "cases")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

testGroupWithName :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm String -> Phantoms.TTerm Testing.TestGroup
testGroupWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "subgroups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGroupWithDescription :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Testing.TestGroup
testGroupWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "subgroups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGroupWithSubgroups :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestGroup] -> Phantoms.TTerm Testing.TestGroup
testGroupWithSubgroups original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "cases")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

testGroupWithCases :: Phantoms.TTerm Testing.TestGroup -> Phantoms.TTerm [Testing.TestCaseWithMetadata] -> Phantoms.TTerm Testing.TestGroup
testGroupWithCases original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TestGroup"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "subgroups"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TestGroup"),
              Core.projectionField = (Core.Name "subgroups")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "cases"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCheckingTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeCheckingTestCase
typeCheckingTestCase input outputTerm outputType =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "outputTerm"),
          Core.fieldTerm = (Phantoms.unTTerm outputTerm)},
        Core.Field {
          Core.fieldName = (Core.Name "outputType"),
          Core.fieldTerm = (Phantoms.unTTerm outputType)}]}))

typeCheckingTestCaseInput :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Term
typeCheckingTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCheckingTestCaseOutputTerm :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Term
typeCheckingTestCaseOutputTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
        Core.projectionField = (Core.Name "outputTerm")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCheckingTestCaseOutputType :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Type
typeCheckingTestCaseOutputType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
        Core.projectionField = (Core.Name "outputType")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCheckingTestCaseWithInput :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TypeCheckingTestCase
typeCheckingTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outputTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "outputTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outputType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "outputType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeCheckingTestCaseWithOutputTerm :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TypeCheckingTestCase
typeCheckingTestCaseWithOutputTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outputTerm"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "outputType"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "outputType")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeCheckingTestCaseWithOutputType :: Phantoms.TTerm Testing.TypeCheckingTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeCheckingTestCase
typeCheckingTestCaseWithOutputType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outputTerm"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingTestCase"),
              Core.projectionField = (Core.Name "outputTerm")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "outputType"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeCheckingFailureTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TypeCheckingFailureTestCase
typeCheckingFailureTestCase input =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingFailureTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)}]}))

typeCheckingFailureTestCaseInput :: Phantoms.TTerm Testing.TypeCheckingFailureTestCase -> Phantoms.TTerm Core.Term
typeCheckingFailureTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeCheckingFailureTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeCheckingFailureTestCaseWithInput :: Phantoms.TTerm Testing.TypeCheckingFailureTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.TypeCheckingFailureTestCase
typeCheckingFailureTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeCheckingFailureTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

topologicalSortBindingsTestCase :: Phantoms.TTerm [(Core.Name, Core.Term)] -> Phantoms.TTerm [[(Core.Name, Core.Term)]] -> Phantoms.TTerm Testing.TopologicalSortBindingsTestCase
topologicalSortBindingsTestCase bindings expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm bindings)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

topologicalSortBindingsTestCaseBindings :: Phantoms.TTerm Testing.TopologicalSortBindingsTestCase -> Phantoms.TTerm [(Core.Name, Core.Term)]
topologicalSortBindingsTestCaseBindings x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
        Core.projectionField = (Core.Name "bindings")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortBindingsTestCaseExpected :: Phantoms.TTerm Testing.TopologicalSortBindingsTestCase -> Phantoms.TTerm [[(Core.Name, Core.Term)]]
topologicalSortBindingsTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortBindingsTestCaseWithBindings :: Phantoms.TTerm Testing.TopologicalSortBindingsTestCase -> Phantoms.TTerm [(Core.Name, Core.Term)] -> Phantoms.TTerm Testing.TopologicalSortBindingsTestCase
topologicalSortBindingsTestCaseWithBindings original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topologicalSortBindingsTestCaseWithExpected :: Phantoms.TTerm Testing.TopologicalSortBindingsTestCase -> Phantoms.TTerm [[(Core.Name, Core.Term)]] -> Phantoms.TTerm Testing.TopologicalSortBindingsTestCase
topologicalSortBindingsTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "bindings"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortBindingsTestCase"),
              Core.projectionField = (Core.Name "bindings")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

topologicalSortTestCase :: Phantoms.TTerm [(Int, [Int])] -> Phantoms.TTerm (Either [[Int]] [Int]) -> Phantoms.TTerm Testing.TopologicalSortTestCase
topologicalSortTestCase adjacencyList expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Phantoms.unTTerm adjacencyList)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

topologicalSortTestCaseAdjacencyList :: Phantoms.TTerm Testing.TopologicalSortTestCase -> Phantoms.TTerm [(Int, [Int])]
topologicalSortTestCaseAdjacencyList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
        Core.projectionField = (Core.Name "adjacencyList")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortTestCaseExpected :: Phantoms.TTerm Testing.TopologicalSortTestCase -> Phantoms.TTerm (Either [[Int]] [Int])
topologicalSortTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortTestCaseWithAdjacencyList :: Phantoms.TTerm Testing.TopologicalSortTestCase -> Phantoms.TTerm [(Int, [Int])] -> Phantoms.TTerm Testing.TopologicalSortTestCase
topologicalSortTestCaseWithAdjacencyList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topologicalSortTestCaseWithExpected :: Phantoms.TTerm Testing.TopologicalSortTestCase -> Phantoms.TTerm (Either [[Int]] [Int]) -> Phantoms.TTerm Testing.TopologicalSortTestCase
topologicalSortTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortTestCase"),
              Core.projectionField = (Core.Name "adjacencyList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

topologicalSortSCCTestCase :: Phantoms.TTerm [(Int, [Int])] -> Phantoms.TTerm [[Int]] -> Phantoms.TTerm Testing.TopologicalSortSCCTestCase
topologicalSortSCCTestCase adjacencyList expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Phantoms.unTTerm adjacencyList)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

topologicalSortSCCTestCaseAdjacencyList :: Phantoms.TTerm Testing.TopologicalSortSCCTestCase -> Phantoms.TTerm [(Int, [Int])]
topologicalSortSCCTestCaseAdjacencyList x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
        Core.projectionField = (Core.Name "adjacencyList")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortSCCTestCaseExpected :: Phantoms.TTerm Testing.TopologicalSortSCCTestCase -> Phantoms.TTerm [[Int]]
topologicalSortSCCTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

topologicalSortSCCTestCaseWithAdjacencyList :: Phantoms.TTerm Testing.TopologicalSortSCCTestCase -> Phantoms.TTerm [(Int, [Int])] -> Phantoms.TTerm Testing.TopologicalSortSCCTestCase
topologicalSortSCCTestCaseWithAdjacencyList original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

topologicalSortSCCTestCaseWithExpected :: Phantoms.TTerm Testing.TopologicalSortSCCTestCase -> Phantoms.TTerm [[Int]] -> Phantoms.TTerm Testing.TopologicalSortSCCTestCase
topologicalSortSCCTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "adjacencyList"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TopologicalSortSCCTestCase"),
              Core.projectionField = (Core.Name "adjacencyList")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

serializationTestCase :: Phantoms.TTerm Ast.Expr -> Phantoms.TTerm String -> Phantoms.TTerm Testing.SerializationTestCase
serializationTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

serializationTestCaseInput :: Phantoms.TTerm Testing.SerializationTestCase -> Phantoms.TTerm Ast.Expr
serializationTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

serializationTestCaseOutput :: Phantoms.TTerm Testing.SerializationTestCase -> Phantoms.TTerm String
serializationTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

serializationTestCaseWithInput :: Phantoms.TTerm Testing.SerializationTestCase -> Phantoms.TTerm Ast.Expr -> Phantoms.TTerm Testing.SerializationTestCase
serializationTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

serializationTestCaseWithOutput :: Phantoms.TTerm Testing.SerializationTestCase -> Phantoms.TTerm String -> Phantoms.TTerm Testing.SerializationTestCase
serializationTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SerializationTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

simplifyTermTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.SimplifyTermTestCase
simplifyTermTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

simplifyTermTestCaseInput :: Phantoms.TTerm Testing.SimplifyTermTestCase -> Phantoms.TTerm Core.Term
simplifyTermTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simplifyTermTestCaseOutput :: Phantoms.TTerm Testing.SimplifyTermTestCase -> Phantoms.TTerm Core.Term
simplifyTermTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

simplifyTermTestCaseWithInput :: Phantoms.TTerm Testing.SimplifyTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.SimplifyTermTestCase
simplifyTermTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

simplifyTermTestCaseWithOutput :: Phantoms.TTerm Testing.SimplifyTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.SimplifyTermTestCase
simplifyTermTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SimplifyTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

normalizeTypeVariablesTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase
normalizeTypeVariablesTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

normalizeTypeVariablesTestCaseInput :: Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase -> Phantoms.TTerm Core.Term
normalizeTypeVariablesTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalizeTypeVariablesTestCaseOutput :: Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase -> Phantoms.TTerm Core.Term
normalizeTypeVariablesTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

normalizeTypeVariablesTestCaseWithInput :: Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase
normalizeTypeVariablesTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

normalizeTypeVariablesTestCaseWithOutput :: Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.NormalizeTypeVariablesTestCase
normalizeTypeVariablesTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.NormalizeTypeVariablesTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeReductionTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeReductionTestCase
typeReductionTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

typeReductionTestCaseInput :: Phantoms.TTerm Testing.TypeReductionTestCase -> Phantoms.TTerm Core.Type
typeReductionTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeReductionTestCaseOutput :: Phantoms.TTerm Testing.TypeReductionTestCase -> Phantoms.TTerm Core.Type
typeReductionTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeReductionTestCaseWithInput :: Phantoms.TTerm Testing.TypeReductionTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeReductionTestCase
typeReductionTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeReductionTestCaseWithOutput :: Phantoms.TTerm Testing.TypeReductionTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.TypeReductionTestCase
typeReductionTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.TypeReductionTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

writerTestCase :: Phantoms.TTerm a -> Phantoms.TTerm String -> Phantoms.TTerm (Testing.WriterTestCase a)
writerTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.WriterTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

writerTestCaseInput :: Phantoms.TTerm (Testing.WriterTestCase a) -> Phantoms.TTerm a
writerTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.WriterTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

writerTestCaseOutput :: Phantoms.TTerm (Testing.WriterTestCase a) -> Phantoms.TTerm String
writerTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.WriterTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

writerTestCaseWithInput :: Phantoms.TTerm (Testing.WriterTestCase a) -> Phantoms.TTerm a -> Phantoms.TTerm (Testing.WriterTestCase a)
writerTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.WriterTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.WriterTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

writerTestCaseWithOutput :: Phantoms.TTerm (Testing.WriterTestCase a) -> Phantoms.TTerm String -> Phantoms.TTerm (Testing.WriterTestCase a)
writerTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.WriterTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.WriterTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

substInTypeTestCase :: Phantoms.TTerm [(Core.Name, Core.Type)] -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.SubstInTypeTestCase
substInTypeTestCase substitution input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm substitution)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

substInTypeTestCaseSubstitution :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm [(Core.Name, Core.Type)]
substInTypeTestCaseSubstitution x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
        Core.projectionField = (Core.Name "substitution")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substInTypeTestCaseInput :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm Core.Type
substInTypeTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substInTypeTestCaseOutput :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm Core.Type
substInTypeTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

substInTypeTestCaseWithSubstitution :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm [(Core.Name, Core.Type)] -> Phantoms.TTerm Testing.SubstInTypeTestCase
substInTypeTestCaseWithSubstitution original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

substInTypeTestCaseWithInput :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.SubstInTypeTestCase
substInTypeTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "substitution")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

substInTypeTestCaseWithOutput :: Phantoms.TTerm Testing.SubstInTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.SubstInTypeTestCase
substInTypeTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "substitution"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "substitution")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.SubstInTypeTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

variableOccursInTypeTestCase :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Bool -> Phantoms.TTerm Testing.VariableOccursInTypeTestCase
variableOccursInTypeTestCase variable type_ expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm variable)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

variableOccursInTypeTestCaseVariable :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Core.Name
variableOccursInTypeTestCaseVariable x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
        Core.projectionField = (Core.Name "variable")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableOccursInTypeTestCaseType :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Core.Type
variableOccursInTypeTestCaseType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableOccursInTypeTestCaseExpected :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Bool
variableOccursInTypeTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

variableOccursInTypeTestCaseWithVariable :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Testing.VariableOccursInTypeTestCase
variableOccursInTypeTestCaseWithVariable original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableOccursInTypeTestCaseWithType :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.VariableOccursInTypeTestCase
variableOccursInTypeTestCaseWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

variableOccursInTypeTestCaseWithExpected :: Phantoms.TTerm Testing.VariableOccursInTypeTestCase -> Phantoms.TTerm Bool -> Phantoms.TTerm Testing.VariableOccursInTypeTestCase
variableOccursInTypeTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "variable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "variable")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.VariableOccursInTypeTestCase"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unshadowVariablesTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.UnshadowVariablesTestCase
unshadowVariablesTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

unshadowVariablesTestCaseInput :: Phantoms.TTerm Testing.UnshadowVariablesTestCase -> Phantoms.TTerm Core.Term
unshadowVariablesTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unshadowVariablesTestCaseOutput :: Phantoms.TTerm Testing.UnshadowVariablesTestCase -> Phantoms.TTerm Core.Term
unshadowVariablesTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unshadowVariablesTestCaseWithInput :: Phantoms.TTerm Testing.UnshadowVariablesTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.UnshadowVariablesTestCase
unshadowVariablesTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unshadowVariablesTestCaseWithOutput :: Phantoms.TTerm Testing.UnshadowVariablesTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.UnshadowVariablesTestCase
unshadowVariablesTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnshadowVariablesTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unifyTypesTestCase :: Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Either String Typing.TypeSubst) -> Phantoms.TTerm Testing.UnifyTypesTestCase
unifyTypesTestCase schemaTypes left right expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Phantoms.unTTerm schemaTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

unifyTypesTestCaseSchemaTypes :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm [Core.Name]
unifyTypesTestCaseSchemaTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
        Core.projectionField = (Core.Name "schemaTypes")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unifyTypesTestCaseLeft :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm Core.Type
unifyTypesTestCaseLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unifyTypesTestCaseRight :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm Core.Type
unifyTypesTestCaseRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unifyTypesTestCaseExpected :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm (Either String Typing.TypeSubst)
unifyTypesTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unifyTypesTestCaseWithSchemaTypes :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Testing.UnifyTypesTestCase
unifyTypesTestCaseWithSchemaTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unifyTypesTestCaseWithLeft :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.UnifyTypesTestCase
unifyTypesTestCaseWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "schemaTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unifyTypesTestCaseWithRight :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.UnifyTypesTestCase
unifyTypesTestCaseWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "schemaTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

unifyTypesTestCaseWithExpected :: Phantoms.TTerm Testing.UnifyTypesTestCase -> Phantoms.TTerm (Either String Typing.TypeSubst) -> Phantoms.TTerm Testing.UnifyTypesTestCase
unifyTypesTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "schemaTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "schemaTypes")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.UnifyTypesTestCase"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

joinTypesTestCase :: Phantoms.TTerm Core.Type -> Phantoms.TTerm Core.Type -> Phantoms.TTerm (Either () [Typing.TypeConstraint]) -> Phantoms.TTerm Testing.JoinTypesTestCase
joinTypesTestCase left right expected =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm expected)}]}))

joinTypesTestCaseLeft :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm Core.Type
joinTypesTestCaseLeft x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
        Core.projectionField = (Core.Name "left")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinTypesTestCaseRight :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm Core.Type
joinTypesTestCaseRight x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
        Core.projectionField = (Core.Name "right")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinTypesTestCaseExpected :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm (Either () [Typing.TypeConstraint])
joinTypesTestCaseExpected x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
        Core.projectionField = (Core.Name "expected")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

joinTypesTestCaseWithLeft :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.JoinTypesTestCase
joinTypesTestCaseWithLeft original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

joinTypesTestCaseWithRight :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm Core.Type -> Phantoms.TTerm Testing.JoinTypesTestCase
joinTypesTestCaseWithRight original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "expected")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

joinTypesTestCaseWithExpected :: Phantoms.TTerm Testing.JoinTypesTestCase -> Phantoms.TTerm (Either () [Typing.TypeConstraint]) -> Phantoms.TTerm Testing.JoinTypesTestCase
joinTypesTestCaseWithExpected original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "left")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.JoinTypesTestCase"),
              Core.projectionField = (Core.Name "right")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "expected"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

validateCoreTermTestCase :: Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core_.InvalidTermError) -> Phantoms.TTerm Testing.ValidateCoreTermTestCase
validateCoreTermTestCase input output =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm input)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm output)}]}))

validateCoreTermTestCaseInput :: Phantoms.TTerm Testing.ValidateCoreTermTestCase -> Phantoms.TTerm Core.Term
validateCoreTermTestCaseInput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
        Core.projectionField = (Core.Name "input")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

validateCoreTermTestCaseOutput :: Phantoms.TTerm Testing.ValidateCoreTermTestCase -> Phantoms.TTerm (Maybe Core_.InvalidTermError)
validateCoreTermTestCaseOutput x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
        Core.projectionField = (Core.Name "output")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

validateCoreTermTestCaseWithInput :: Phantoms.TTerm Testing.ValidateCoreTermTestCase -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Testing.ValidateCoreTermTestCase
validateCoreTermTestCaseWithInput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
              Core.projectionField = (Core.Name "output")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

validateCoreTermTestCaseWithOutput :: Phantoms.TTerm Testing.ValidateCoreTermTestCase -> Phantoms.TTerm (Maybe Core_.InvalidTermError) -> Phantoms.TTerm Testing.ValidateCoreTermTestCase
validateCoreTermTestCaseWithOutput original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "input"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.testing.ValidateCoreTermTestCase"),
              Core.projectionField = (Core.Name "input")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "output"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
