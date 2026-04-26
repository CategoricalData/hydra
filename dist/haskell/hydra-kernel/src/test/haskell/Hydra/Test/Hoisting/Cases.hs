-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for subterm hoisting and case statement hoisting

module Hydra.Test.Hoisting.Cases where
import qualified Hydra.Core as Core
import qualified Hydra.Hoisting as Hoisting
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | Test cases for subterm hoisting and case statement hoisting
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "hoistCases",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "hoistSubterms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistNothing: simple let unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\_ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistNothing: let with list in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\_ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermVariable (Core.Name "x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistNothing: let with application in body unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\_ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermVariable (Core.Name "g")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: list in body is hoisted into local let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: multiple lists in body are hoisted together",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_2"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "pair")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_2"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: list in binding value is hoisted into local let",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: nested lists hoisted from inside out",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_2"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermVariable (Core.Name "_hoist_x_body_1"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_2"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistApplications: application in list element is hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermApplication _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}),
                    (Core.TermVariable (Core.Name "y"))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_x_body_1"),
                      (Core.TermVariable (Core.Name "y"))])}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistApplications: application in record field is hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermApplication _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}]}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = (Core.Name "Data"),
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "value"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}]}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistApplications: nested applications hoisted from inside out",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermApplication _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_2"),
                        Core.bindingTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_x_body_2")])}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistCaseStatements: case in application argument is hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermCases _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistCaseStatements: case in list element is hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermCases _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermList [
                    Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Result"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "ok"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                        Core.Field {
                          Core.fieldName = (Core.Name "err"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Result"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "ok"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                            Core.Field {
                              Core.fieldName = (Core.Name "err"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "_hoist_x_body_1")])}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: nested let - inner let processed independently",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                        (Core.TermVariable (Core.Name "y"))])}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                          Core.bindingName = (Core.Name "_hoist_y_body_1"),
                          Core.bindingTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermVariable (Core.Name "y"))]),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_y_body_1"))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: non-let term is unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistApplications: bare application unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermApplication _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: term referring to let-bound variable needs no capture",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: term referring to lambda above let needs no capture",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLambda (Core.Lambda {
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
                        (Core.TermVariable (Core.Name "x"))])}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLambda (Core.Lambda {
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
                          Core.bindingName = (Core.Name "_hoist_x_body_1"),
                          Core.bindingTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "y"),
                            (Core.TermVariable (Core.Name "x"))]),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var not free in hoisted term needs no capture",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: lambda-bound var free in hoisted term requires capture",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))])}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermList [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermVariable (Core.Name "y"))])})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_x_body_1")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: only free lambda-bound vars are captured",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermVariable (Core.Name "b"))])}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "b"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermList [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermVariable (Core.Name "b"))])})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "a"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "b"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_x_body_1")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))}))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: stable naming for binding and body",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))]),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: stable naming for multiple bindings",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hoistLists: polymorphic binding with self-reference below hoisted term",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistSubterms (\pt -> case (Pairs.second pt) of
                  Core.TermList _ -> True
                  _ -> False) Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
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
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermList [
                                Core.TermVariable (Core.Name "x"),
                                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermLambda (Core.Lambda {
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
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "hoistCaseStatements",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case at top level of let body is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermCases (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Optional"),
                    Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "just"),
                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                      Core.Field {
                        Core.fieldName = (Core.Name "nothing"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in let binding value is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside lambda body is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside nested lambdas is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "b"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Result"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "ok"),
                                Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                              Core.Field {
                                Core.fieldName = (Core.Name "err"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "b"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Result"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "ok"),
                                Core.fieldTerm = (Core.TermVariable (Core.Name "b"))},
                              Core.Field {
                                Core.fieldName = (Core.Name "err"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case as LHS of one application is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case wrapped in annotation is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.annotatedTermAnnotation = M.empty})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.annotatedTermAnnotation = M.empty})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in lambda with one application is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case as RHS of application IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in nested application LHS IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "z"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "w"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "w"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "z"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "w"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "w"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside list element IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermVariable (Core.Name "_hoist_f_1")])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside lambda inside list IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "list inside lambda is NOT hoisted (only case statements)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "a"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "a"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in binding is not hoisted, case in arg position is hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "w"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "w"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "_hoist_x_body_1"),
                        Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "w"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "w"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_x_body_1"))}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in nested let body is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                    Core.letBody = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "w"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "w"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                    Core.letBody = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "z"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "w"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "w"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in let inside lambda is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in lambda inside let body is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                      Core.caseStatementTypeName = (Core.Name "Optional"),
                      Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                      Core.caseStatementCases = [
                        Core.Field {
                          Core.fieldName = (Core.Name "just"),
                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                        Core.Field {
                          Core.fieldName = (Core.Name "nothing"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case with let+lambda+app is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "x"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in triple application LHS IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "a"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "c"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))}))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "c"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "z"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "a"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "c"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))}))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "c"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case as second argument IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in both arguments - both hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                        Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "x"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing},
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_2"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_1"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_f_2"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in second list element IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                        (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                          (Core.TermVariable (Core.Name "_hoist_f_1"))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "multiple cases in list - all hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermList [
                        Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}),
                        (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))]),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "x"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing},
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_2"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermList [
                          Core.TermVariable (Core.Name "_hoist_f_1"),
                          (Core.TermVariable (Core.Name "_hoist_f_2"))])})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in pair first element IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermPair (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "_hoist_f_1"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in pair second element IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = Nothing,
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermVariable (Core.Name "_hoist_f_1"))))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in child let binding hoisted into child",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "outer"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "inner"),
                            Core.bindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = Nothing,
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                                  Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.Name "Optional"),
                                    Core.caseStatementDefault = Nothing,
                                    Core.caseStatementCases = [
                                      Core.Field {
                                        Core.fieldName = (Core.Name "just"),
                                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "y"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                      Core.Field {
                                        Core.fieldName = (Core.Name "nothing"),
                                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                                  Core.bindingType = Nothing}],
                              Core.letBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_inner_1"))}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in child let body hoisted into child",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
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
                          Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
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
                              Core.bindingName = (Core.Name "_hoist_inner_body_1"),
                              Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = Nothing,
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_inner_body_1"))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case at top level of child let NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "outer"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "inner"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "outer"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "inner"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermVariable (Core.Name "inner"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "cases in both outer and child - each hoisted locally",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "outer"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "x"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                        Core.applicationArgument = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "inner"),
                              Core.bindingTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                                  Core.caseStatementTypeName = (Core.Name "Optional"),
                                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                                  Core.caseStatementCases = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "just"),
                                      Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "y"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                    Core.Field {
                                      Core.fieldName = (Core.Name "nothing"),
                                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]}))})),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermVariable (Core.Name "inner"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "outer"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_outer_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "x"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
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
                                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                                        Core.caseStatementTypeName = (Core.Name "Optional"),
                                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "b"))),
                                        Core.caseStatementCases = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "just"),
                                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "y"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                          Core.Field {
                                            Core.fieldName = (Core.Name "nothing"),
                                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}]})),
                                      Core.bindingType = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "_hoist_inner_1"))}))})),
                                Core.bindingType = Nothing}],
                            Core.letBody = (Core.TermVariable (Core.Name "inner"))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "outer"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "lambda after app LHS takes us out of top level",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "y"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside case branch is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case inside case default branch is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "y"))),
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "b"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in arg position inside case branch IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                        Core.caseStatementTypeName = (Core.Name "Optional"),
                        Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                        Core.caseStatementCases = [
                          Core.Field {
                            Core.fieldName = (Core.Name "just"),
                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermCases (Core.CaseStatement {
                                  Core.caseStatementTypeName = (Core.Name "Optional"),
                                  Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                  Core.caseStatementCases = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "just"),
                                      Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "b"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                    Core.Field {
                                      Core.fieldName = (Core.Name "nothing"),
                                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))}))}))},
                          Core.Field {
                            Core.fieldName = (Core.Name "nothing"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "a"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermCases (Core.CaseStatement {
                                Core.caseStatementTypeName = (Core.Name "Optional"),
                                Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "a"))),
                                Core.caseStatementCases = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "just"),
                                    Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.Name "b"),
                                      Core.lambdaDomain = Nothing,
                                      Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "nothing"),
                                    Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = (Just (Core.TermVariable (Core.Name "x"))),
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in let body inside applied case default IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLet (Core.Let {
                                Core.letBindings = [
                                  Core.Binding {
                                    Core.bindingName = (Core.Name "b"),
                                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                    Core.bindingType = Nothing}],
                                Core.letBody = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                    Core.caseStatementTypeName = (Core.Name "Result"),
                                    Core.caseStatementDefault = Nothing,
                                    Core.caseStatementCases = [
                                      Core.Field {
                                        Core.fieldName = (Core.Name "ok"),
                                        Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                          Core.lambdaParameter = (Core.Name "y"),
                                          Core.lambdaDomain = Nothing,
                                          Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                      Core.Field {
                                        Core.fieldName = (Core.Name "err"),
                                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "a"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLet (Core.Let {
                                Core.letBindings = [
                                  Core.Binding {
                                    Core.bindingName = (Core.Name "b"),
                                    Core.bindingTerm = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                    Core.bindingType = Nothing}],
                                Core.letBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "_hoist_b_body_1"),
                                      Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                                        Core.caseStatementTypeName = (Core.Name "Result"),
                                        Core.caseStatementDefault = Nothing,
                                        Core.caseStatementCases = [
                                          Core.Field {
                                            Core.fieldName = (Core.Name "ok"),
                                            Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                              Core.lambdaParameter = (Core.Name "y"),
                                              Core.lambdaDomain = Nothing,
                                              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                          Core.Field {
                                            Core.fieldName = (Core.Name "err"),
                                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                                      Core.bindingType = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_b_body_1")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))}))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case in let body inside applied case branch IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "b"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                      Core.bindingType = Nothing}],
                                  Core.letBody = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                      Core.caseStatementTypeName = (Core.Name "Result"),
                                      Core.caseStatementDefault = Nothing,
                                      Core.caseStatementCases = [
                                        Core.Field {
                                          Core.fieldName = (Core.Name "ok"),
                                          Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                            Core.lambdaParameter = (Core.Name "y"),
                                            Core.lambdaDomain = Nothing,
                                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                        Core.Field {
                                          Core.fieldName = (Core.Name "err"),
                                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "a"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLet (Core.Let {
                                  Core.letBindings = [
                                    Core.Binding {
                                      Core.bindingName = (Core.Name "b"),
                                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                                      Core.bindingType = Nothing}],
                                  Core.letBody = (Core.TermLet (Core.Let {
                                    Core.letBindings = [
                                      Core.Binding {
                                        Core.bindingName = (Core.Name "_hoist_b_body_1"),
                                        Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                                          Core.caseStatementTypeName = (Core.Name "Result"),
                                          Core.caseStatementDefault = Nothing,
                                          Core.caseStatementCases = [
                                            Core.Field {
                                              Core.fieldName = (Core.Name "ok"),
                                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                                Core.lambdaParameter = (Core.Name "y"),
                                                Core.lambdaDomain = Nothing,
                                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                            Core.Field {
                                              Core.fieldName = (Core.Name "err"),
                                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                                        Core.bindingType = Nothing}],
                                    Core.letBody = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_b_body_1")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))}))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case application at top level of binding is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                          Core.caseStatementTypeName = (Core.Name "Optional"),
                          Core.caseStatementDefault = Nothing,
                          Core.caseStatementCases = [
                            Core.Field {
                              Core.fieldName = (Core.Name "just"),
                              Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "y"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                            Core.Field {
                              Core.fieldName = (Core.Name "nothing"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case application in arg position IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case application inside immediately-applied lambda IS hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "a"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "_hoist_f_1"),
                            Core.bindingTerm = (Core.TermCases (Core.CaseStatement {
                              Core.caseStatementTypeName = (Core.Name "Optional"),
                              Core.caseStatementDefault = Nothing,
                              Core.caseStatementCases = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "just"),
                                  Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                    Core.lambdaParameter = (Core.Name "y"),
                                    Core.lambdaDomain = Nothing,
                                    Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "nothing"),
                                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                            Core.bindingType = Nothing}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "_hoist_f_1")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "case application in lambda body is NOT hoisted",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (ShowCore.term (Hoisting.hoistCaseStatements Lexical.emptyGraph (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))),
                Testing.universalTestCaseExpected = (ShowCore.term (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "f"),
                      Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                            Core.caseStatementTypeName = (Core.Name "Optional"),
                            Core.caseStatementDefault = Nothing,
                            Core.caseStatementCases = [
                              Core.Field {
                                Core.fieldName = (Core.Name "just"),
                                Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                  Core.lambdaParameter = (Core.Name "y"),
                                  Core.lambdaDomain = Nothing,
                                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))},
                              Core.Field {
                                Core.fieldName = (Core.Name "nothing"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}]})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "f"))})))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
