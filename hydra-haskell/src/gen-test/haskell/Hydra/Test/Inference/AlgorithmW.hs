-- Note: this is an automatically generated file. Do not edit.

-- | Algorithm W inference tests

module Hydra.Test.Inference.AlgorithmW where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Algorithm W test cases
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Algorithm W test cases",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    testGroupForSystemF],
  Testing.testGroupCases = []}

testGroupForSystemF :: Testing.TestGroup
testGroupForSystemF = Testing.TestGroup {
  Testing.testGroupName = "STLC to System F",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#1",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
          Core.lambdaParameter = (Core.Name "x"),
          Core.lambdaDomain = Nothing,
          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0"],
          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#2",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "foo"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#3",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "f"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#4",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "f"),
              Core.bindingTerm = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "f"))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#5",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "sng"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x")])}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "sng"))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0"],
          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
            Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#6",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "sng"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermList [
                  Core.TermVariable (Core.Name "x")])}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}), (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "alice"))}))))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
            Core.pairTypeSecond = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#7",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
          Core.letBindings = [
            Core.Binding {
              Core.bindingName = (Core.Name "+"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "x"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "y"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "+")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermVariable (Core.Name "+")),
              Core.applicationArgument = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
                Core.applicationArgument = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
            Core.applicationArgument = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.negate"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [],
          Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#9",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
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
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermVariable (Core.Name "f"))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0"],
          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
            Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#10",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
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
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}))),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "g"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "xx"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "yy"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "xx"))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "f"), (Core.TermVariable (Core.Name "g"))))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0",
            (Core.Name "t1")],
          Core.typeSchemeType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
            Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#11",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
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
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}))),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "g"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "u"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "f"), (Core.TermVariable (Core.Name "g"))))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0",
            Core.Name "t1",
            Core.Name "t2",
            (Core.Name "t3")],
          Core.typeSchemeType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))})),
            Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t3"))}))}))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#12",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
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
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))}))),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "g"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "u"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermVariable (Core.Name "v"))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "f"), (Core.TermVariable (Core.Name "g"))))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0",
            (Core.Name "t1")],
          Core.typeSchemeType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
            Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#13",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
        Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
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
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))}))),
              Core.bindingType = Nothing},
            Core.Binding {
              Core.bindingName = (Core.Name "g"),
              Core.bindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "u"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "v"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})))}))),
              Core.bindingType = Nothing}],
          Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "f"), (Core.TermVariable (Core.Name "g"))))})),
        Testing.inferenceTestCaseOutput = Core.TypeScheme {
          Core.typeSchemeVariables = [
            Core.Name "t0",
            (Core.Name "t1")],
          Core.typeSchemeType = (Core.TypePair (Core.PairType {
            Core.pairTypeFirst = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))})),
            Core.pairTypeSecond = (Core.TypeFunction (Core.FunctionType {
              Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))})),
          Core.typeSchemeConstraints = Nothing}})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}
