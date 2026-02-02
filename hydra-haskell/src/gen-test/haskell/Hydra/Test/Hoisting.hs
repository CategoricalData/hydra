-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for hoisting transformations

module Hydra.Test.Hoisting where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for hoisting transformations
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "hoisting",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "hoistSubterms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: simple let unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: let with list in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistNothing: let with application in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateNothing,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: list in body is hoisted into local let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: multiple lists in body are hoisted together",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_2"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_2"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: list in binding value is hoisted into local let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: nested lists hoisted from inside out",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_2"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist__body_1"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_2"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: application in list element is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}),
                (Core.TermVariable (Core.Name "y"))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist__body_1"),
                  (Core.TermVariable (Core.Name "y"))])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: application in record field is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermRecord (Core.Record {
                Core.recordTypeName = (Core.Name "Data"),
                Core.recordFields = [
                  Core.Field {
                    Core.fieldName = (Core.Name "value"),
                    Core.fieldTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Data"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "_hoist__body_1"))}]}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: nested applications hoisted from inside out",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_2"),
                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))})),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist__body_2")])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistCaseStatements: case in application argument is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateCaseStatements,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistCaseStatements: case in list element is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateCaseStatements,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Result"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "ok"),
                      Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                    Core.Field {
                      Core.fieldName = (Core.Name "err"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Result"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "ok"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "err"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "_hoist__body_1")])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: nested let - inner let processed independently",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermVariable (Core.Name "y"))])}))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "_hoist__body_1"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: non-let term is unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistApplications: bare application unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateApplications,
            Testing.hoistSubtermsTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: term referring to let-bound variable needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: term referring to lambda above let needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "y"),
                    (Core.TermVariable (Core.Name "x"))])}))}))}))),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "_hoist__body_1"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermVariable (Core.Name "y"),
                        (Core.TermVariable (Core.Name "x"))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var not free in hoisted term needs no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var free in hoisted term requires capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermVariable (Core.Name "y"))])}))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))])}))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist__body_1")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: only free lambda-bound vars are captured",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "b"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "b"))])}))})))})))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "b"))])}))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "a"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "b"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist__body_1")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))})))})))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: stable naming for binding and body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: stable naming for multiple bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_1"))}))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_y_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_y_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hoistLists: polymorphic binding with self-reference below hoisted term",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistSubterms (Testing.HoistSubtermsTestCase {
            Testing.hoistSubtermsTestCasePredicate = Testing.HoistPredicateLists,
            Testing.hoistSubtermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.hoistSubtermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermList [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistCaseStatements",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case at top level of let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "Optional"),
                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "just"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "nothing"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                Core.caseStatementTypeName = (Core.Name "Optional"),
                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                Core.caseStatementCases = [
                  Core.Field {
                    Core.fieldName = (Core.Name "just"),
                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                  Core.Field {
                    Core.fieldName = (Core.Name "nothing"),
                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in let binding value is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside lambda body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside nested lambdas is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Result"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "ok"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "err"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as LHS of one application is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case wrapped in annotation is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.annotatedTermAnnotation = M.empty})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in lambda with one application is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as RHS of application IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in nested application LHS IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "w"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "w"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "z"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "w"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "w"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside list element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_f_1")])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside lambda inside list IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list inside lambda is NOT hoisted (only case statements)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "a"),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "a"),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in binding is not hoisted, case in arg position is hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "_hoist__body_1"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "w"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in nested let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "w"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "w"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in let inside lambda is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in lambda inside let body is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "a"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = (Core.Name "Optional"),
                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "just"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "nothing"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case with let+lambda+app is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in triple application LHS IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "c"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "c"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))})))}]})))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "z"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "c"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))})))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "c"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})))})))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case as second argument IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in both arguments - both hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_2"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in second list element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = Nothing,
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermVariable (Core.Name "_hoist_f_1"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple cases in list - all hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))),
                    (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_f_1"),
                      (Core.TermVariable (Core.Name "_hoist_f_2"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in pair first element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermPair (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "_hoist_f_1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in pair second element IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermVariable (Core.Name "_hoist_f_1"))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in child let binding hoisted into child",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "_hoist_inner_1"),
                              Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = Nothing,
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_inner_1"))}))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in child let body hoisted into child",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "_hoist__body_1"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist__body_1"))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case at top level of child let NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "inner"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cases in both outer and child - each hoisted locally",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                    Core.applicationArgument = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "inner"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "inner"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_outer_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_outer_1"))})),
                      Core.applicationArgument = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "inner"),
                            Core.bindingTerm = (Core.TermLet (Core.Let {
                              Core.letBindings = [
                                Core.Binding {
                                  Core.bindingName = (Core.Name "_hoist_inner_1"),
                                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.Name "Optional"),
                                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                                    Core.caseStatementCases = [
                                      Core.Field {
                                        Core.fieldName = (Core.Name "just"),
                                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "y"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                                      Core.Field {
                                        Core.fieldName = (Core.Name "nothing"),
                                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})))),
                                  Core.bindingType = Nothing}],
                              Core.letBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_inner_1"))}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))}))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda after app LHS takes us out of top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside case branch is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case inside case default branch is NOT hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "a"))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "b"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case in arg position inside case branch IS hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistCaseStatements (Testing.HoistCaseStatementsTestCase {
            Testing.hoistCaseStatementsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))})))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.hoistCaseStatementsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_f_1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "b"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "b"))})))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))}))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))})))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistLetBindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested let inside lambda: binding hoisted with lambda capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistLetBindings (Testing.HoistLetBindingsTestCase {
            Testing.hoistLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "g"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))},
            Testing.hoistLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "type application: nested let outside lambda CAN be hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistLetBindings (Testing.HoistLetBindingsTestCase {
            Testing.hoistLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "y"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))},
            Testing.hoistLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermTypeApplication (Core.TypeApplicationTerm {
                    Core.typeApplicationTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "f_y"))}))}))),
                    Core.typeApplicationTermType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistPolymorphicLetBindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no polymorphic bindings: simple let unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no polymorphic bindings: multiple monomorphic bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hi")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hi")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single polymorphic binding: already at top level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding inside lambda: no capture",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "id"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f_id")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_id"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding captures lambda variable: wrapped in lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding captures multiple lambda variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "g"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "triple")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "c"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "c"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "c"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "b"),
                        Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "triple")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "c"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "c"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding captures some but not all lambda variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "g"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "c"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                                Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                  Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "c"))}))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "c"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "c"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "c"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding captures both lambda-bound and let-bound variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [],
                            Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                            Core.typeSchemeConstraints = Nothing}))},
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [],
                            Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "sibling polymorphic bindings inside lambda: one calls the other",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "a"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                            Core.typeSchemeConstraints = Nothing}))},
                        Core.Binding {
                          Core.bindingName = (Core.Name "h"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "z"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_h")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "outer"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_h"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "outer"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "z"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "sibling polymorphic bindings inside lambda: h passes its own args to g",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "t"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "a"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
                            Core.typeSchemeConstraints = Nothing}))},
                        Core.Binding {
                          Core.bindingName = (Core.Name "h"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "v"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "t"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_h")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "outer"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "t"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))})))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_h"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "outer"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "v"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "t"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "outer"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))})))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "untyped binding: not hoisted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no name collision: distinct names after unshadowing",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "id2"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "b"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f_id2")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_id2"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested polymorphic binding calls enclosing polymorphic binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "a"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "inner"),
                        Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "h"),
                              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "z"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))),
                              Core.bindingType = (Just (Core.TypeScheme {
                                Core.typeSchemeVariables = [
                                  Core.Name "b"],
                                Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                                Core.typeSchemeConstraints = Nothing}))}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "outer"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "inner"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_h")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_h"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding captures monomorphic sibling in same let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "left"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "right"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "sleft"),
                            Core.bindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "left"))})),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                              Core.typeSchemeConstraints = Nothing}))},
                          Core.Binding {
                            Core.bindingName = (Core.Name "sright"),
                            Core.bindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "right"))})),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                              Core.typeSchemeConstraints = Nothing}))},
                          Core.Binding {
                            Core.bindingName = (Core.Name "cannotUnify"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "sright"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "a"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "cannotUnify")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "left"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "right"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "sleft"),
                            Core.bindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "left"))})),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                              Core.typeSchemeConstraints = Nothing}))},
                          Core.Binding {
                            Core.bindingName = (Core.Name "sright"),
                            Core.bindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "right"))})),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [],
                              Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_cannotUnify")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "sright"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_cannotUnify"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "sleft"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "sright"),
                        Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "sright"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lets: poly binding references poly sibling from outer let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "left"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "sleft"),
                          Core.bindingTerm = (Core.TermVariable (Core.Name "left")),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [],
                            Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "cannotUnify"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "a"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "joinList"),
                              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "cannotUnify")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                              Core.bindingType = (Just (Core.TypeScheme {
                                Core.typeSchemeVariables = [
                                  Core.Name "b"],
                                Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                                  Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                                Core.typeSchemeConstraints = Nothing}))}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "joinList")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "left"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "sleft"),
                          Core.bindingTerm = (Core.TermVariable (Core.Name "left")),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [],
                            Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_joinList")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_cannotUnify"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "a"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "sleft"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "wrapper_joinList"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "b"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "sleft"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper_cannotUnify")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "sleft"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "b"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "wrapper")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "hoistPolymorphicTypeParameters",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested function types: all type variables must be declared",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "choose"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "forLeft"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "forRight"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "e"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "forLeft")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})))})))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0",
                            Core.Name "t1",
                            (Core.Name "t2")],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermVariable (Core.Name "choose"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "f_choose")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_choose"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t0"),
                    Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t1"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t2"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "forLeft"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "forRight"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "e"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "forLeft")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})))})))})))}))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      Core.Name "t1",
                      (Core.Name "t2")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "type variable in return position only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "returnT"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "unit"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = Core.TypeUnit,
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermVariable (Core.Name "returnT"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = Core.TypeUnit,
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "f_returnT")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = Core.TypeUnit,
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_returnT"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "unit"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = Core.TypeUnit,
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "type variables in deeply nested generics",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "nested"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0",
                            Core.Name "t1",
                            (Core.Name "t2")],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypePair (Core.PairType {
                              Core.pairTypeFirst = (Core.TypePair (Core.PairType {
                                Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                                Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))})),
                              Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t2"))})),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermVariable (Core.Name "nested"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "f_nested")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                        Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeBoolean)})),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_nested"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t0"),
                    Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t1"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t2"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))}))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      Core.Name "t1",
                      (Core.Name "t2")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))})),
                        Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t2"))})),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple bindings with overlapping type variable names",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id1"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                          Core.typeSchemeConstraints = Nothing}))},
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "id1"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "id2"))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                      Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "outer"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "outer_id1"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "outer_id2"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
                      Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "outer_id1"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "outer_id2"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "outer"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "captured variable with type parameters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "g"),
                          Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.bindingType = (Just (Core.TypeScheme {
                            Core.typeSchemeVariables = [
                              Core.Name "t"],
                            Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                              Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                                Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t"))}))})),
                            Core.typeSchemeConstraints = Nothing}))}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f_g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                        Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.pairTypeSecond = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t"),
                    Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = (Just (Core.TypeLiteral Core.LiteralTypeString)),
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))}))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "short type variable names are treated as type parameters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "g"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "t"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "s",
                            Core.Name "t",
                            (Core.Name "v")],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "s")),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "v"))}))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermVariable (Core.Name "g"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "f_g")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "s"),
                    Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "v"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "s"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "t"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))})))}))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "s",
                      Core.Name "t",
                      (Core.Name "v")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "s")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "v"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numbered type variables like t0 t1 t2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "g"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0",
                            Core.Name "t1",
                            (Core.Name "t2")],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t2"))}))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermVariable (Core.Name "g"))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "f_g")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "f_g"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t0"),
                    Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t1"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t2"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "undefined"))})))})))}))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      Core.Name "t1",
                      (Core.Name "t2")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t2"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "choose pattern from mutateTrace",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseHoistPolymorphicLetBindings (Testing.HoistPolymorphicLetBindingsTestCase {
            Testing.hoistPolymorphicLetBindingsTestCaseInput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "mutateTrace"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "mutate"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "restore"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "f"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "choose"),
                              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "forLeft"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "forRight"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "e"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "forLeft")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})))})))}))),
                              Core.bindingType = (Just (Core.TypeScheme {
                                Core.typeSchemeVariables = [
                                  Core.Name "t0",
                                  Core.Name "t1",
                                  (Core.Name "t2")],
                                Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                  Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                                  Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                                Core.typeSchemeConstraints = Nothing}))}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "choose")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "forLeft"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "forRight"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))})))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "mutateTrace"))},
            Testing.hoistPolymorphicLetBindingsTestCaseOutput = Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "mutateTrace"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "mutate"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "restore"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "f"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "mutateTrace_choose")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "forLeft"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "forRight"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))},
                Core.Binding {
                  Core.bindingName = (Core.Name "mutateTrace_choose"),
                  Core.bindingTerm = (Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name "t0"),
                    Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                      Core.typeLambdaParameter = (Core.Name "t1"),
                      Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                        Core.typeLambdaParameter = (Core.Name "t2"),
                        Core.typeLambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "forLeft"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "forRight"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "e"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "forLeft")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))})))})))})))}))}))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      Core.Name "t1",
                      (Core.Name "t2")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermVariable (Core.Name "mutateTrace"))}})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
