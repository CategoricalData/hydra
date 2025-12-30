-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for term rewriting operations

module Hydra.Test.Rewriting where

import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Test cases for term rewriting operations
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "rewriting",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "freeVariables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string literal has no free variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermLiteral (Core.LiteralString "foo")),
            Testing.freeVariablesTestCaseOutput = S.empty})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermVariable (Core.Name "x")),
            Testing.freeVariablesTestCaseOutput = (S.fromList [
              Core.Name "x"])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bound variable is not free",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
            Testing.freeVariablesTestCaseOutput = S.empty})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unbound variable in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Testing.freeVariablesTestCaseOutput = (S.fromList [
              Core.Name "x"])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed free and bound variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermList [
              Core.TermVariable (Core.Name "x"),
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))]),
            Testing.freeVariablesTestCaseOutput = (S.fromList [
              Core.Name "x"])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple free variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFreeVariables (Testing.FreeVariablesTestCase {
            Testing.freeVariablesTestCaseInput = (Core.TermList [
              Core.TermVariable (Core.Name "x"),
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))]),
            Testing.freeVariablesTestCaseOutput = (S.fromList [
              Core.Name "x",
              (Core.Name "y")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "simplifyTerm",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "const application with literal",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSimplifyTerm (Testing.SimplifyTermTestCase {
            Testing.simplifyTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.simplifyTermTestCaseOutput = (Core.TermLiteral (Core.LiteralString "foo"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSimplifyTerm (Testing.SimplifyTermTestCase {
            Testing.simplifyTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermVariable (Core.Name "x"))])}))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
            Testing.simplifyTermTestCaseOutput = (Core.TermList [
              Core.TermVariable (Core.Name "y"),
              (Core.TermVariable (Core.Name "y"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unused parameter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSimplifyTerm (Testing.SimplifyTermTestCase {
            Testing.simplifyTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
            Testing.simplifyTermTestCaseOutput = (Core.TermLiteral (Core.LiteralString "foo"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lambda applications",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseSimplifyTerm (Testing.SimplifyTermTestCase {
            Testing.simplifyTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermVariable (Core.Name "a"))])}))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))})),
            Testing.simplifyTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo"),
              (Core.TermVariable (Core.Name "y"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "flattenLetTerms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-let term unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFlattenLetTerms (Testing.FlattenLetTermsTestCase {
            Testing.flattenLetTermsTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.flattenLetTermsTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "list term unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFlattenLetTerms (Testing.FlattenLetTermsTestCase {
            Testing.flattenLetTermsTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo")]),
            Testing.flattenLetTermsTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-nested let unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFlattenLetTerms (Testing.FlattenLetTermsTestCase {
            Testing.flattenLetTermsTestCaseInput = (Core.TermLet (Core.Let {
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
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermVariable (Core.Name "y"))])}))})),
            Testing.flattenLetTermsTestCaseOutput = (Core.TermLet (Core.Let {
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
                Core.letBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x"),
                  (Core.TermVariable (Core.Name "y"))])}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested binding in let value is flattened",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFlattenLetTerms (Testing.FlattenLetTermsTestCase {
            Testing.flattenLetTermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "a"),
                (Core.TermVariable (Core.Name "b"))])})),
            Testing.flattenLetTermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "b_x"),
                    (Core.TermVariable (Core.Name "b_y"))]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "a"),
                (Core.TermVariable (Core.Name "b"))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple levels of nesting are flattened",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFlattenLetTerms (Testing.FlattenLetTermsTestCase {
            Testing.flattenLetTermsTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                        Core.bindingType = Nothing},
                      Core.Binding {
                        Core.bindingName = (Core.Name "y"),
                        Core.bindingTerm = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.bindingName = (Core.Name "p"),
                              Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                              Core.bindingType = Nothing},
                            Core.Binding {
                              Core.bindingName = (Core.Name "q"),
                              Core.bindingTerm = (Core.TermList [
                                Core.TermVariable (Core.Name "x"),
                                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                              Core.bindingType = Nothing}],
                          Core.letBody = (Core.TermList [
                            Core.TermVariable (Core.Name "a"),
                            (Core.TermVariable (Core.Name "q"))])})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "a"),
                (Core.TermVariable (Core.Name "b"))])})),
            Testing.flattenLetTermsTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "b_x"),
                    (Core.TermVariable (Core.Name "b_y"))]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_y"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "a"),
                    (Core.TermVariable (Core.Name "b_y_q"))]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_y_p"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b_y_q"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "b_x"),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "a"),
                (Core.TermVariable (Core.Name "b"))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "liftLambdaAboveLet",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple let with lambda in body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bare lambda unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bare let unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with let in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with two nested lambdas",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "z"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda inside let body already above let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let without lambda in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple let bindings with lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "z"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "z"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing},
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested lets with lambda at innermost level",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "z"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "z"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "y"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda between two lets",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "hello")),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple lambdas between nested lets",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "b"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "a"))}))})))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "a"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "b"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "a"))}))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple lambdas already above let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))}))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation above let containing lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
              Core.annotatedTermAnnotation = M.empty}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation above lambda in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.annotatedTermAnnotation = M.empty}))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))})),
                Core.annotatedTermAnnotation = M.empty}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation between two lambdas",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.annotatedTermAnnotation = M.empty}))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "z"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermVariable (Core.Name "x"))})),
                  Core.annotatedTermAnnotation = M.empty}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation on the body of lambda in let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermVariable (Core.Name "x")),
                  Core.annotatedTermAnnotation = M.empty}))})))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                  Core.annotatedTermBody = (Core.TermVariable (Core.Name "x")),
                  Core.annotatedTermAnnotation = M.empty}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotation on lambda already above let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
              Core.annotatedTermAnnotation = M.empty}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda inside a list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda in multiple list elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermList [
              Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}),
              (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "z"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "w"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))}))]),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermList [
              Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
              (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "w"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "z"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda in a let binding value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "x"),
                        Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))})),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "x"),
                          Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda inside a pair",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermPair (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}), (Core.TermLiteral (Core.LiteralString "test")))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})), (Core.TermLiteral (Core.LiteralString "test"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda in both elements of a pair",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermPair (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "y"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}), (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "z"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "w"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermPair (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "y"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "x"))}))})), (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "w"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "z"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let-lambda inside lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseLiftLambdaAboveLet (Testing.LiftLambdaAboveLetTestCase {
            Testing.liftLambdaAboveLetTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "outer"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "x"),
                    Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "inner"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))}))),
            Testing.liftLambdaAboveLetTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "outer"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "inner"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.bindingName = (Core.Name "x"),
                      Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermVariable (Core.Name "x"))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "deannotateTerm",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated literal unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.deannotateTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated variable unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermVariable (Core.Name "x")),
            Testing.deannotateTermTestCaseOutput = (Core.TermVariable (Core.Name "x"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated lambda unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
            Testing.deannotateTermTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single annotation stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.deannotateTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested annotations stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                Core.annotatedTermAnnotation = M.empty})),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.deannotateTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated lambda stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.deannotateTermTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated application stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateTerm (Testing.DeannotateTermTestCase {
            Testing.deannotateTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.deannotateTermTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "deannotateType",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated primitive type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated string type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeLiteral Core.LiteralTypeString),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeLiteral Core.LiteralTypeString)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unannotated function type unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single annotation stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.annotatedTypeAnnotation = M.empty})),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested annotations stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeAnnotated (Core.AnnotatedType {
                Core.annotatedTypeBody = (Core.TypeLiteral Core.LiteralTypeString),
                Core.annotatedTypeAnnotation = M.empty})),
              Core.annotatedTypeAnnotation = M.empty})),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeLiteral Core.LiteralTypeString)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated list type stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
              Core.annotatedTypeAnnotation = M.empty})),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "annotated function type stripped",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseDeannotateType (Testing.DeannotateTypeTestCase {
            Testing.deannotateTypeTestCaseInput = (Core.TypeAnnotated (Core.AnnotatedType {
              Core.annotatedTypeBody = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.annotatedTypeAnnotation = M.empty})),
            Testing.deannotateTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "topologicalSortBindings",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "isolated bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortBindings (Testing.TopologicalSortBindingsTestCase {
            Testing.topologicalSortBindingsTestCaseBindings = [
              (Core.Name "a", (Core.TermLiteral (Core.LiteralString "foo"))),
              (Core.Name "b", (Core.TermLiteral (Core.LiteralString "bar")))],
            Testing.topologicalSortBindingsTestCaseExpected = [
              [
                (Core.Name "a", (Core.TermLiteral (Core.LiteralString "foo")))],
              [
                (Core.Name "b", (Core.TermLiteral (Core.LiteralString "bar")))]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single recursive binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortBindings (Testing.TopologicalSortBindingsTestCase {
            Testing.topologicalSortBindingsTestCaseBindings = [
              (Core.Name "a", (Core.TermList [
                Core.TermVariable (Core.Name "a")]))],
            Testing.topologicalSortBindingsTestCaseExpected = [
              [
                (Core.Name "a", (Core.TermList [
                  Core.TermVariable (Core.Name "a")]))]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mutually recursive bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortBindings (Testing.TopologicalSortBindingsTestCase {
            Testing.topologicalSortBindingsTestCaseBindings = [
              (Core.Name "a", (Core.TermList [
                Core.TermVariable (Core.Name "b")])),
              (Core.Name "b", (Core.TermList [
                Core.TermVariable (Core.Name "a")]))],
            Testing.topologicalSortBindingsTestCaseExpected = [
              [
                (Core.Name "a", (Core.TermList [
                  Core.TermVariable (Core.Name "b")])),
                (Core.Name "b", (Core.TermList [
                  Core.TermVariable (Core.Name "a")]))]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseTopologicalSortBindings (Testing.TopologicalSortBindingsTestCase {
            Testing.topologicalSortBindingsTestCaseBindings = [
              (Core.Name "a", (Core.TermVariable (Core.Name "b"))),
              (Core.Name "b", (Core.TermList [
                Core.TermVariable (Core.Name "a"),
                (Core.TermVariable (Core.Name "c"))])),
              (Core.Name "c", (Core.TermLiteral (Core.LiteralString "foo"))),
              (Core.Name "d", (Core.TermLiteral (Core.LiteralString "bar")))],
            Testing.topologicalSortBindingsTestCaseExpected = [
              [
                (Core.Name "c", (Core.TermLiteral (Core.LiteralString "foo")))],
              [
                (Core.Name "a", (Core.TermVariable (Core.Name "b"))),
                (Core.Name "b", (Core.TermList [
                  Core.TermVariable (Core.Name "a"),
                  (Core.TermVariable (Core.Name "c"))]))],
              [
                (Core.Name "d", (Core.TermLiteral (Core.LiteralString "bar")))]]})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "normalizeTypeVariables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "literal without type variables unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "simple let without type annotations unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with monomorphic type scheme unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with monomorphic binding referencing string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding with free type variable unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "monomorphic binding with typed lambda unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic binding with typed lambda in body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "bar")),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeType = (Core.TypeVariable (Core.Name "a")),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic identity function normalized",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
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
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "polymorphic const function normalized",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "const"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a",
                      (Core.Name "b")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "const")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "const"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      (Core.Name "t1")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "const")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "binding rewriting does not affect body with typed lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
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
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = (Just (Core.TypeFunction (Core.FunctionType {
                  Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                  Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))),
                Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested polymorphic lets normalized",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
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
              Core.letBody = (Core.TermLet (Core.Let {
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
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "id2"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                    Core.bindingType = (Just (Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                      Core.typeSchemeConstraints = Nothing}))}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "nested same substitution in bindings and environment",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
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
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "id2"),
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
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "id2"),
                    Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                    Core.bindingType = (Just (Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                      Core.typeSchemeConstraints = Nothing}))}],
                Core.letBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "parent type variable shadows child variable",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t1"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no shadowing distinct type variables",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "b"))),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "b"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "b"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "id2"),
                        Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t1"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))})),
                          Core.typeSchemeConstraints = Nothing}))}],
                    Core.letBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id2")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))})),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0"],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "locally free type variable in nested binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseNormalizeTypeVariables (Testing.NormalizeTypeVariablesTestCase {
            Testing.normalizeTypeVariablesTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "fun1"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "a"))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "b"))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "fun2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "c"))),
                              Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "z"), (Core.TermVariable (Core.Name "y"))))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "c"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "c")),
                                Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "c")),
                                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "fun2")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "a",
                      (Core.Name "b")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "b")),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeVariable (Core.Name "a")),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "b"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "fun1")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
            Testing.normalizeTypeVariablesTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "fun1"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t0"))),
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t1"))),
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.bindingName = (Core.Name "fun2"),
                            Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "t2"))),
                              Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "z"), (Core.TermVariable (Core.Name "y"))))}))),
                            Core.bindingType = (Just (Core.TypeScheme {
                              Core.typeSchemeVariables = [
                                Core.Name "t2"],
                              Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                                Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                                  Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t2")),
                                  Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))})),
                              Core.typeSchemeConstraints = Nothing}))}],
                        Core.letBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "fun2")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                  Core.bindingType = (Just (Core.TypeScheme {
                    Core.typeSchemeVariables = [
                      Core.Name "t0",
                      (Core.Name "t1")],
                    Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                        Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                          Core.pairTypeFirst = (Core.TypeVariable (Core.Name "t0")),
                          Core.pairTypeSecond = (Core.TypeVariable (Core.Name "t1"))}))}))})),
                    Core.typeSchemeConstraints = Nothing}))}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "fun1")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "etaExpandTerm",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "integer literal unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
            Testing.etaExpansionTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo"),
              (Core.TermLiteral (Core.LiteralString "bar"))]),
            Testing.etaExpansionTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo"),
              (Core.TermLiteral (Core.LiteralString "bar"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fully applied binary function unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda with fully applied primitive unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lambda returning constant unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bare unary primitive unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "bare binary primitive unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partially applied binary primitive expands to one lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
              Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v1"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "foo"))})),
                Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "projection expands to lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "Person"),
              Core.projectionField = (Core.Name "firstName")})))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "v1"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                  Core.projectionTypeName = (Core.Name "Person"),
                  Core.projectionField = (Core.Name "firstName")})))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partial application inside lambda expands",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
            Testing.etaExpansionTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v1"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with constant body unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "let with bare primitive value unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "foo"))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "foo"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "foo"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "fully applied unary unchanged",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))})),
            Testing.etaExpansionTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "FOO"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "partial application in list expands",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEtaExpansion (Testing.EtaExpansionTestCase {
            Testing.etaExpansionTestCaseInput = (Core.TermList [
              Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")])})),
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))]),
            Testing.etaExpansionTestCaseOutput = (Core.TermList [
              Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")])})),
              (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "v1"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})),
                  Core.applicationArgument = (Core.TermVariable (Core.Name "v1"))}))})))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "foldOverTerm",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect labels from single node - pre-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectLabels,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect labels from tree - pre-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
              Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
              (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectLabels,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "b"),
              Core.TermLiteral (Core.LiteralString "c"),
              (Core.TermLiteral (Core.LiteralString "d"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect labels from single node - post-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList []))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPost,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectLabels,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect labels from tree - post-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "a"), (Core.TermList [
              Core.TermPair (Core.TermLiteral (Core.LiteralString "b"), (Core.TermList [])),
              (Core.TermPair (Core.TermLiteral (Core.LiteralString "c"), (Core.TermList [
                Core.TermPair (Core.TermLiteral (Core.LiteralString "d"), (Core.TermList []))])))]))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPost,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectLabels,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "b"),
              Core.TermLiteral (Core.LiteralString "d"),
              Core.TermLiteral (Core.LiteralString "c"),
              (Core.TermLiteral (Core.LiteralString "a"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "sum int32 literals",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))}))]),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 52)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect list lengths - pre-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralString "foo"),
                (Core.TermLiteral (Core.LiteralString "bar"))],
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "quux")])}))]),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectListLengths,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect list lengths - post-order",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralString "foo"),
                (Core.TermLiteral (Core.LiteralString "bar"))],
              (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "quux")])}))]),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPost,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectListLengths,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "rewriteType",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String type in left side of either is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String type in right side of either is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String types in both sides of either are replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
              Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String type in nested either (left of left) is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String type in nested either (right of right) is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
              Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)}))})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
              Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String types in complex nested either are all replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                Core.eitherTypeRight = (Core.TypeLiteral Core.LiteralTypeString)})),
              Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral Core.LiteralTypeString),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeEither (Core.EitherType {
              Core.eitherTypeLeft = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})),
              Core.eitherTypeRight = (Core.TypeEither (Core.EitherType {
                Core.eitherTypeLeft = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.eitherTypeRight = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String in list type is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String in function domain is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String in function codomain is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
              Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64)),
              Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "String in optional type is replaced",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteType (Testing.RewriteTypeTestCase {
            Testing.rewriteTypeTestCaseInput = (Core.TypeMaybe (Core.TypeLiteral Core.LiteralTypeString)),
            Testing.rewriteTypeTestCaseRewriter = Testing.TypeRewriterReplaceStringWithInt32,
            Testing.rewriteTypeTestCaseOutput = (Core.TypeMaybe (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "rewriteTerm",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string literal foo replaced with bar",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLiteral (Core.LiteralString "foo")),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLiteral (Core.LiteralString "bar"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in variable not changed",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermVariable (Core.Name "x")),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermVariable (Core.Name "x"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo"),
              (Core.TermLiteral (Core.LiteralString "baz"))]),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "bar"),
              (Core.TermLiteral (Core.LiteralString "baz"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple strings in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "foo"),
              Core.TermLiteral (Core.LiteralString "foo"),
              (Core.TermLiteral (Core.LiteralString "baz"))]),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "bar"),
              Core.TermLiteral (Core.LiteralString "bar"),
              (Core.TermLiteral (Core.LiteralString "baz"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in optional (just)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "foo")))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralString "bar"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in function application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "print")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in nested applications",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in record field",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Person"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "name"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Person"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "name"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "strings in multiple record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Data"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "a"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                Core.Field {
                  Core.fieldName = (Core.Name "b"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "c"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Data"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "a"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                Core.Field {
                  Core.fieldName = (Core.Name "b"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "c"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in pair",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "foo"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermPair (Core.TermLiteral (Core.LiteralString "bar"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in let binding value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in first case branch",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in second case branch",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in default branch",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "foo"))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = (Just (Core.TermLiteral (Core.LiteralString "bar"))),
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "success"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))},
                Core.Field {
                  Core.fieldName = (Core.Name "error"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string deeply nested in record in list in application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
              Core.applicationArgument = (Core.TermList [
                Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Item"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]})])})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "process")),
              Core.applicationArgument = (Core.TermList [
                Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "Item"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "value"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]})])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in union inject value",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "Result"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "success"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermUnion (Core.Injection {
              Core.injectionTypeName = (Core.Name "Result"),
              Core.injectionField = Core.Field {
                Core.fieldName = (Core.Name "success"),
                Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in wrapped term",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = (Core.Name "Email"),
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = (Core.Name "Email"),
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in annotated term body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
              Core.annotatedTermAnnotation = M.empty}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in first of multiple let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in second of multiple let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "y"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "baz")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "y"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in all let bindings and body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralString "bar")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in set",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralString "baz"),
              (Core.TermLiteral (Core.LiteralString "foo"))])),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralString "bar"),
              (Core.TermLiteral (Core.LiteralString "baz"))]))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in type lambda body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "a"),
              Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "a"),
              Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in type application body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "foo")),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralString "bar")),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in nested type lambdas",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "a"),
              Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = (Core.Name "b"),
                Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "a"),
              Core.typeLambdaBody = (Core.TermTypeLambda (Core.TypeLambda {
                Core.typeLambdaParameter = (Core.Name "b"),
                Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralString "bar"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in case branch within let binding",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "handler"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Result"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "ok"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "err"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "handler"))})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "handler"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                    Core.caseStatementTypeName = (Core.Name "Result"),
                    Core.caseStatementDefault = Nothing,
                    Core.caseStatementCases = [
                      Core.Field {
                        Core.fieldName = (Core.Name "ok"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))},
                      Core.Field {
                        Core.fieldName = (Core.Name "err"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "baz"))}]})))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "handler"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string in annotated wrapped record field",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseRewriteTerm (Testing.RewriteTermTestCase {
            Testing.rewriteTermTestCaseInput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "User"),
                Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "UserData"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}]}))})),
              Core.annotatedTermAnnotation = M.empty})),
            Testing.rewriteTermTestCaseRewriter = Testing.TermRewriterReplaceFooWithBar,
            Testing.rewriteTermTestCaseOutput = (Core.TermAnnotated (Core.AnnotatedTerm {
              Core.annotatedTermBody = (Core.TermWrap (Core.WrappedTerm {
                Core.wrappedTermTypeName = (Core.Name "User"),
                Core.wrappedTermBody = (Core.TermRecord (Core.Record {
                  Core.recordTypeName = (Core.Name "UserData"),
                  Core.recordFields = [
                    Core.Field {
                      Core.fieldName = (Core.Name "name"),
                      Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}]}))})),
              Core.annotatedTermAnnotation = M.empty}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "rewriteAndFoldTermWithPath",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through application - sum literals",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through nested applications",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermVariable (Core.Name "y"))])})))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through let bindings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermVariable (Core.Name "x"),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)))])})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through record fields",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermRecord (Core.Record {
              Core.recordTypeName = (Core.Name "Point"),
              Core.recordFields = [
                Core.Field {
                  Core.fieldName = (Core.Name "x"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))},
                Core.Field {
                  Core.fieldName = (Core.Name "y"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 20)))}]})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 30)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through case branches",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
              Core.caseStatementTypeName = (Core.Name "Result"),
              Core.caseStatementDefault = Nothing,
              Core.caseStatementCases = [
                Core.Field {
                  Core.fieldName = (Core.Name "ok"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))},
                Core.Field {
                  Core.fieldName = (Core.Name "err"),
                  Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}]})))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through pair",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7))))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 12)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through optional",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermMaybe (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through wrapped term",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermWrap (Core.WrappedTerm {
              Core.wrappedTermTypeName = (Core.Name "Age"),
              Core.wrappedTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 25)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through type lambda",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermTypeLambda (Core.TypeLambda {
              Core.typeLambdaParameter = (Core.Name "a"),
              Core.typeLambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through type application",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermTypeApplication (Core.TypeApplicationTerm {
              Core.typeApplicationTermBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50))),
              Core.typeApplicationTermType = (Core.TypeLiteral Core.LiteralTypeString)})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "path tracking through set elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermSet (S.fromList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "deep nesting - application in lambda in let",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationSumInt32Literals,
            Testing.foldOverTermTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 15)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect list lengths in nested structure",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])]),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectListLengths,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "collect list lengths in let body",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseFoldOverTerm (Testing.FoldOverTermTestCase {
            Testing.foldOverTermTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "xs"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.foldOverTermTestCaseTraversalOrder = Coders.TraversalOrderPre,
            Testing.foldOverTermTestCaseOperation = Testing.FoldOperationCollectListLengths,
            Testing.foldOverTermTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
