-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for expected failures

module Hydra.Test.Inference.Failures where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Expected failure tests
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Expected failures",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    undefinedVariableTests,
    unificationFailureTests,
    invalidApplicationTests,
    selfApplicationTests,
    arityMismatchTests,
    recursiveTypeTests,
    occurCheckTests,
    typeConstructorMisuseTests,
    polymorphismViolationTests,
    letBindingMismatchTests,
    constraintSolverEdgeCaseTests,
    primitiveTypeErrorTests,
    complexConstraintFailureTests],
  Testing.testGroupCases = []}

undefinedVariableTests :: Testing.TestGroup
undefinedVariableTests = Testing.TestGroup {
  Testing.testGroupName = "Undefined variable",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Basic unbound variables",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermVariable (Core.Name "x"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "y"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Unbound in let expressions",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "y")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "y")),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "z"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "z")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Shadowing scope errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "z"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermLet (Core.Let {
                Core.letBindings = [
                  Core.Binding {
                    Core.bindingName = (Core.Name "y"),
                    Core.bindingTerm = (Core.TermVariable (Core.Name "x")),
                    Core.bindingType = Nothing}],
                Core.letBody = (Core.TermVariable (Core.Name "z"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

unificationFailureTests :: Testing.TestGroup
unificationFailureTests = Testing.TestGroup {
  Testing.testGroupName = "Unification failure",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Basic type mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermLiteral (Core.LiteralString "foo"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))],
              (Core.TermLiteral (Core.LiteralString "foo"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "foo")))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Collection type mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a list"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermList [
                Core.TermLiteral (Core.LiteralString "foo")])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermPair (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))], (Core.TermList [
                Core.TermLiteral (Core.LiteralString "foo")]))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "foo")])])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Conditional type mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Polymorphic instantiation conflicts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermPair (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "cons"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "cons")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "cons")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

invalidApplicationTests :: Testing.TestGroup
invalidApplicationTests = Testing.TestGroup {
  Testing.testGroupName = "Invalid application",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Non-function application",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLiteral (Core.LiteralString "foo")),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLiteral (Core.LiteralBoolean True)),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 3.14))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Collection application",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "foo")))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermList []),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)), (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))))))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "index"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Primitive misapplication",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.empty"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.empty"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermMaybe Nothing),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "value"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermList []),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

selfApplicationTests :: Testing.TestGroup
selfApplicationTests = Testing.TestGroup {
  Testing.testGroupName = "Self-application",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Direct self-application",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "x"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Indirect self-application",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "a"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "cycle"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "cycle"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "cycle")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "cycle"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

arityMismatchTests :: Testing.TestGroup
arityMismatchTests = Testing.TestGroup {
  Testing.testGroupName = "Arity mismatch",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Too many arguments",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 999)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))])})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "extra"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Wrong argument types with extra args",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "extra"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.not"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "arg"))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

recursiveTypeTests :: Testing.TestGroup
recursiveTypeTests = Testing.TestGroup {
  Testing.testGroupName = "Recursive type construction",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Direct recursive types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "x")]),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "x")))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Recursive function types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "f"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "f")])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Mutually recursive types",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "y")]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "x"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermVariable (Core.Name "a")),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "a"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermVariable (Core.Name "g")]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermPair (Core.TermVariable (Core.Name "f"), (Core.TermVariable (Core.Name "f")))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

occurCheckTests :: Testing.TestGroup
occurCheckTests = Testing.TestGroup {
  Testing.testGroupName = "Occur check failures",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Function occur checks",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "h"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "g"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "h"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "g"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Mutual occur checks",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "g"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "f"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "a"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "cycle1"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "cycle2")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "cycle1"))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "cycle2"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "cycle1")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "cycle1"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Complex occur checks",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "omega"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "omega"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "omega"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "loop"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "loop")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "loop"))}))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "loop"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

typeConstructorMisuseTests :: Testing.TestGroup
typeConstructorMisuseTests = Testing.TestGroup {
  Testing.testGroupName = "Type constructor misuse",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "List constructor errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a list"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "String constructor errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "foo")])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a list"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Math constructor errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.div"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

polymorphismViolationTests :: Testing.TestGroup
polymorphismViolationTests = Testing.TestGroup {
  Testing.testGroupName = "Polymorphism violations",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Identity function violations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "id"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermPair (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Constrained polymorphism violations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermList [
                      Core.TermVariable (Core.Name "x"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralString "constant"))))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bad"))}))}))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "h"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "incompatible"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Higher-order polymorphism violations",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "f"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermPair (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}), (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "g"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermList [
                Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}),
                (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bad"))}))])})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
              Core.lambdaParameter = (Core.Name "h"),
              Core.lambdaDomain = Nothing,
              Core.lambdaBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "error"))}))}))})))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

letBindingMismatchTests :: Testing.TestGroup
letBindingMismatchTests = Testing.TestGroup {
  Testing.testGroupName = "Let binding type mismatches",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Application type mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "x"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "y"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "result"))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "extra"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "g"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "num"),
                  Core.bindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "num")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "num"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Collection type mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "list1"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "list2"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "list1"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "list2"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "nums"),
                  Core.bindingTerm = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "mixed"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bad"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "nums"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "mixed"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "pair1"),
                  Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)), (Core.TermLiteral (Core.LiteralString "foo")))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "pair2"),
                  Core.bindingTerm = (Core.TermPair (Core.TermLiteral (Core.LiteralString "bar"), (Core.TermVariable (Core.Name "pair1")))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.pairs.first"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "pair2"))}))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Function binding mismatches",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "add"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "badCall"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "add")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "badCall"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

constraintSolverEdgeCaseTests :: Testing.TestGroup
constraintSolverEdgeCaseTests = Testing.TestGroup {
  Testing.testGroupName = "Constraint solver edge cases",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Complex constraint propagation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "complex"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "g"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "complex")),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "a"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "b"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "foo"))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Fixed point combinators",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "fix"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "fix")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "y"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "rec"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "n"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "rec"))}))})))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "omega"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "omega")),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "omega"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Constraint cycles",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "a"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "c"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "b"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "y"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "c")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "c"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "z"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "circular"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "circular"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "circular")),
                Core.applicationArgument = (Core.TermVariable (Core.Name "circular"))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

primitiveTypeErrorTests :: Testing.TestGroup
primitiveTypeErrorTests = Testing.TestGroup {
  Testing.testGroupName = "Primitive function type errors",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Logic primitive errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.and"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.or"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not boolean"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Collection primitive errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maps.lookup"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a map"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.sets.member"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a list"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.maybes.fromMaybe"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not optional"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "Math primitive errors",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.div"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#4",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mod"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

complexConstraintFailureTests :: Testing.TestGroup
complexConstraintFailureTests = Testing.TestGroup {
  Testing.testGroupName = "Complex constraint failures",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Multi-level constraint conflicts",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "x"), (Core.TermVariable (Core.Name "y"))))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "a"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "h"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "z"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermVariable (Core.Name "h")),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "weird"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "weird")),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "y"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermPair (Core.TermVariable (Core.Name "y"), (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#3",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "nested"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "g"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))}))})))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "int_f"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "str_g"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "s"),
                        (Core.TermLiteral (Core.LiteralString "!"))])}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "nested")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "int_f"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "str_g"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForPython"]}]},
    Testing.TestGroup {
      Testing.testGroupName = "Function composition failures",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "triple"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "increment"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "n"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "stringify"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermVariable (Core.Name "s"),
                        (Core.TermLiteral (Core.LiteralString "!"))])}))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "triple")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "increment"))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "stringify"))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = [
            Testing.Tag "disabledForPython"]},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "#2",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
            Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
              Core.letBindings = [
                Core.Binding {
                  Core.bindingName = (Core.Name "compose"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "f"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "g"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "reverse_compose"),
                  Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "g"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "f"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})))}))),
                  Core.bindingType = Nothing},
                Core.Binding {
                  Core.bindingName = (Core.Name "bad"),
                  Core.bindingTerm = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "compose")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "reverse_compose"))})),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length")))})),
                  Core.bindingType = Nothing}],
              Core.letBody = (Core.TermVariable (Core.Name "bad"))}))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
