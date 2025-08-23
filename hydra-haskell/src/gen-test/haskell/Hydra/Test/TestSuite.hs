-- | Test cases for primitive functions

module Hydra.Test.TestSuite where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Mantle as Mantle
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "All tests",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    formattingTests,
    inferenceTests,
    primitiveTests],
  Testing.testGroupCases = []}

formattingTests :: Testing.TestGroup
formattingTests = Testing.TestGroup {
  Testing.testGroupName = "formatting tests",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [],
  Testing.testGroupCases = [
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#1 (lower_snake_case -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD_42_A42_42A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#2 (lower_snake_case -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#3 (lower_snake_case -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#4 (lower_snake_case -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "a_hello_world_42_a42_42a_b",
        Testing.caseConversionTestCaseToString = "a_hello_world_42_a42_42a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#5 (UPPER_SNAKE_CASE -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "a_hello_world_42_a42_42a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#6 (UPPER_SNAKE_CASE -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#7 (UPPER_SNAKE_CASE -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#8 (UPPER_SNAKE_CASE -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "A_HELLO_WORLD_42_A42_42A_B",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD_42_A42_42A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#9 (camelCase -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "a_hello_world42_a4242a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#10 (camelCase -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD42_A4242A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#11 (camelCase -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#12 (camelCase -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "aHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#13 (PascalCase -> lower_snake_case)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionLowerSnake,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "a_hello_world42_a4242a_b"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#14 (PascalCase -> UPPER_SNAKE_CASE)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionUpperSnake,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "A_HELLO_WORLD42_A4242A_B"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#15 (PascalCase -> camelCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionCamel,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "aHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []},
    Testing.TestCaseWithMetadata {
      Testing.testCaseWithMetadataName = "#16 (PascalCase -> PascalCase)",
      Testing.testCaseWithMetadataCase = (Testing.TestCaseCaseConversion (Testing.CaseConversionTestCase {
        Testing.caseConversionTestCaseFromConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseToConvention = Mantle.CaseConventionPascal,
        Testing.caseConversionTestCaseFromString = "AHelloWorld42A4242aB",
        Testing.caseConversionTestCaseToString = "AHelloWorld42A4242aB"})),
      Testing.testCaseWithMetadataDescription = Nothing,
      Testing.testCaseWithMetadataTags = []}]}

inferenceTests :: Testing.TestGroup
inferenceTests = Testing.TestGroup {
  Testing.testGroupName = "Inference tests",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "Algebraic terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "List eliminations (folds)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                  Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                      Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                    Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                  Core.applicationArgument = (Core.TermList [
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                    Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                    (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "List terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "List of strings",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermLiteral (Core.LiteralString "bar"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List of lists of strings",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermList [
                        Core.TermLiteral (Core.LiteralString "foo")],
                      (Core.TermList [])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Empty list",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List containing an empty list",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermList []]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambda producing a polymorphic list",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x")])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambda producing a list of integers",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List with repeated variables",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        Core.TermLiteral (Core.LiteralString "foo"),
                        (Core.TermVariable (Core.Name "x"))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Map terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermMap (M.fromList [
                  (Core.TermLiteral (Core.LiteralString "firstName"), (Core.TermLiteral (Core.LiteralString "Arthur"))),
                  (Core.TermLiteral (Core.LiteralString "lastName"), (Core.TermLiteral (Core.LiteralString "Dent")))])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermMap (M.fromList [])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                    Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                    Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Optional terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermOptional Nothing),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0")))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Product terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Empty products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Non-empty, monotyped products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermList [
                        Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)),
                        (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 137.0)))])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                      (Core.TermList [
                        Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)),
                        (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 137.0)))])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Polytyped products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermList [],
                      (Core.TermLiteral (Core.LiteralString "foo"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0")),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermList [])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeList (Core.TypeVariable (Core.Name "t0")))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Pairs",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                      (Core.TermLiteral (Core.LiteralString "foo"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermList [],
                      (Core.TermLiteral (Core.LiteralString "foo"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0")),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermList [],
                      (Core.TermList [])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0")),
                        (Core.TypeList (Core.TypeVariable (Core.Name "t1")))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Set terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermSet (S.fromList [
                  Core.TermLiteral (Core.LiteralBoolean True)])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeBoolean))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermSet (S.fromList [
                  Core.TermSet (S.fromList [])])),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeSet (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Sum terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Singleton sum terms",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 1,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeLiteral Core.LiteralTypeString])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 1,
                      Core.sumTerm = (Core.TermList [])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Non-singleton sum terms",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 2,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeVariable (Core.Name "t0"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 1,
                      Core.sumSize = 2,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeVariable (Core.Name "t0"),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Algorithm W test cases",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
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
                    Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "foo"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#5",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "sng"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x")])}))),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermVariable (Core.Name "sng"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                    Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#6",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "sng"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x")])}))),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermProduct [
                    Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}),
                    (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "alice"))}))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeProduct [
                    Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))])}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#7",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "+"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "+")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))),
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermVariable (Core.Name "+")),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#9",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0"],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#10",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing},
                    Core.Binding {
                      Core.letBindingName = (Core.Name "g"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermProduct [
                    Core.TermVariable (Core.Name "f"),
                    (Core.TermVariable (Core.Name "g"))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeProduct [
                    Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}),
                    (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))])}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#11",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing},
                    Core.Binding {
                      Core.letBindingName = (Core.Name "g"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermProduct [
                    Core.TermVariable (Core.Name "f"),
                    (Core.TermVariable (Core.Name "g"))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    Core.Name "t1",
                    Core.Name "t2",
                    (Core.Name "t3")],
                  Core.typeSchemeType = (Core.TypeProduct [
                    Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}),
                    (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t3"))}))}))])}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#12",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing},
                    Core.Binding {
                      Core.letBindingName = (Core.Name "g"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermProduct [
                    Core.TermVariable (Core.Name "f"),
                    (Core.TermVariable (Core.Name "g"))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeProduct [
                    Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}),
                    (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))])}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#13",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                  Core.letBindings = [
                    Core.Binding {
                      Core.letBindingName = (Core.Name "f"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing},
                    Core.Binding {
                      Core.letBindingName = (Core.Name "g"),
                      Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                      Core.letBindingType = Nothing}],
                  Core.letEnvironment = (Core.TermProduct [
                    Core.TermVariable (Core.Name "f"),
                    (Core.TermVariable (Core.Name "g"))])})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [
                    Core.Name "t0",
                    (Core.Name "t1")],
                  Core.typeSchemeType = (Core.TypeProduct [
                    Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}),
                    (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                      Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))])}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Expected failures",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "y"))}))})),
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
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "y")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "y")),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "z"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "y"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "z")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))])}))})),
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
                            Core.letBindingName = (Core.Name "y"),
                            Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermVariable (Core.Name "z"))}))})))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "y"),
                            Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermVariable (Core.Name "z"))}))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                      Core.applicationFunction = (Core.TermProduct [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                        (Core.TermLiteral (Core.LiteralString "foo"))]),
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
                      Core.applicationFunction = (Core.TermProduct [
                        Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))],
                        (Core.TermList [
                          Core.TermLiteral (Core.LiteralString "foo")])]),
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermList [
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
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermProduct [
                          Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                          (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))]),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))}))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "cons"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermList [
                        Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "cons")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                        (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "cons")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                      Core.applicationFunction = (Core.TermProduct [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                        (Core.TermLiteral (Core.LiteralString "foo"))]),
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
                      Core.applicationFunction = (Core.TermProduct [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]),
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
                      Core.applicationFunction = (Core.TermOptional Nothing),
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
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "y")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "a"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "b"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "a"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "cycle"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "cycle"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "cycle")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "cycle"))}))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "x")]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermVariable (Core.Name "x"))]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "f"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))})))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermList [
                              Core.TermVariable (Core.Name "f")])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
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
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "y")]),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "y"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermVariable (Core.Name "x"),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "a"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "b"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "b"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "a")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "a"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermList [
                            Core.TermVariable (Core.Name "g")]),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermVariable (Core.Name "f"),
                            (Core.TermVariable (Core.Name "f"))]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "h"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "g"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "h"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "g"))}))})),
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "g"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "a"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "b"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "b"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "a"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "cycle1"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "cycle2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "cycle1"))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "cycle2"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "cycle1")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "cycle1"))}))})),
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
                          Core.letBindingName = (Core.Name "omega"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "omega"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "omega"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "loop"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "loop"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
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
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermList [
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
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermProduct [
                          Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                          (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))]),
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermList [
                              Core.TermVariable (Core.Name "x"),
                              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
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
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermProduct [
                              Core.TermVariable (Core.Name "x"),
                              (Core.TermLiteral (Core.LiteralString "constant"))])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                              Core.tupleProjectionArity = 2,
                              Core.tupleProjectionIndex = 0,
                              Core.tupleProjectionDomain = Nothing})))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                            Core.tupleProjectionArity = 2,
                            Core.tupleProjectionIndex = 0,
                            Core.tupleProjectionDomain = Nothing})))),
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
                          Core.letBindingName = (Core.Name "h"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
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
                      Core.lambdaBody = (Core.TermProduct [
                        Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                        (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])})))})),
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
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "y"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "y"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLiteral (Core.LiteralString "result"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "extra"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "g"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "num"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "num")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "num"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
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
                          Core.letBindingName = (Core.Name "list1"),
                          Core.letBindingTerm = (Core.TermList [
                            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))]),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "list2"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "list1"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "list2"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "nums"),
                          Core.letBindingTerm = (Core.TermList [
                            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                            (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))]),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "mixed"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bad"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "nums"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "mixed"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "pair1"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                            (Core.TermLiteral (Core.LiteralString "foo"))]),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "pair2"),
                          Core.letBindingTerm = (Core.TermProduct [
                            Core.TermLiteral (Core.LiteralString "bar"),
                            (Core.TermVariable (Core.Name "pair1"))]),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                              Core.tupleProjectionArity = 2,
                              Core.tupleProjectionIndex = 0,
                              Core.tupleProjectionDomain = Nothing})))),
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
                          Core.letBindingName = (Core.Name "add"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "badCall"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "add")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "not a number"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "badCall"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean True))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "complex"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
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
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
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
                          Core.letBindingName = (Core.Name "fix"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "fix")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "y"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
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
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "omega"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "omega")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "omega"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
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
                          Core.letBindingName = (Core.Name "a"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "b")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "c"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "b"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "c")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "c"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "z"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "a")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
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
                          Core.letBindingName = (Core.Name "circular"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "circular"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "circular")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "circular"))}))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.fromMaybe"))),
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
          Testing.testGroupCases = []},
        Testing.TestGroup {
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
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermProduct [
                                Core.TermVariable (Core.Name "x"),
                                (Core.TermVariable (Core.Name "y"))])})))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "a"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "a"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "a"))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "h"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))})))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
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
                          Core.letBindingName = (Core.Name "weird"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "weird")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermProduct [
                                Core.TermVariable (Core.Name "y"),
                                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])})))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "nested"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "int_f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "n"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "str_g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "s"),
                                (Core.TermLiteral (Core.LiteralString "!"))])}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "nested")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "int_f"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "str_g"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
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
                          Core.letBindingName = (Core.Name "triple"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "increment"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "n"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "n"))})),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "stringify"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "s"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "s"),
                                (Core.TermLiteral (Core.LiteralString "!"))])}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "triple")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "increment"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "stringify"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInferenceFailure (Testing.InferenceFailureTestCase {
                    Testing.inferenceFailureTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "compose"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "reverse_compose"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bad"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "compose")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "reverse_compose"))})),
                              Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length")))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "bad"))}))})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Fundamentals",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Lambdas",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Simple lambdas",
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
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 137)))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Nested lambdas",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))})))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Nested lambdas with shadowing",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))})))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Let terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Simple",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Empty let",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [],
                      Core.letEnvironment = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Trivial let",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Multiple references to a let-bound term",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bar"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermList [
                        Core.TermVariable (Core.Name "foo"),
                        Core.TermVariable (Core.Name "bar"),
                        (Core.TermVariable (Core.Name "foo"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Nested let",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "bar"),
                            Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermList [
                          Core.TermVariable (Core.Name "foo"),
                          (Core.TermVariable (Core.Name "bar"))])}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "bar"),
                            Core.letBindingTerm = (Core.TermProduct [
                              Core.TermVariable (Core.Name "foo"),
                              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))]),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermVariable (Core.Name "bar"))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "sng"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermList [
                              Core.TermVariable (Core.Name "x")])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "foo"),
                            Core.letBindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                            Core.letBindingType = Nothing},
                          Core.Binding {
                            Core.letBindingName = (Core.Name "bar"),
                            Core.letBindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "bar"))})),
                            Core.letBindingType = Nothing},
                          Core.Binding {
                            Core.letBindingName = (Core.Name "quux"),
                            Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "sng")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermProduct [
                          Core.TermVariable (Core.Name "foo"),
                          (Core.TermProduct [
                            Core.TermVariable (Core.Name "bar"),
                            (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "quux")),
                              Core.applicationArgument = (Core.TermList [])}))])])}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        (Core.TypeProduct [
                          Core.TypeList (Core.TypeLiteral Core.LiteralTypeString),
                          (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))])])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Nested let with shadowing",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "foo"),
                            Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermVariable (Core.Name "foo"))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralString "foo")),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bar"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "foo")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "foo"),
                            Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137))),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermProduct [
                          Core.TermVariable (Core.Name "bar"),
                          (Core.TermVariable (Core.Name "foo"))])}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Let-polymorphism",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})])}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                          Core.applicationArgument = (Core.TermList [
                            Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})])}))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                        (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "list"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermList [
                              Core.TermVariable (Core.Name "x")])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "list")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                        (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "list")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))}))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#6",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "singleton"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermList [
                              Core.TermVariable (Core.Name "x")])}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                                  Core.applicationArgument = (Core.TermProduct [
                                    Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "singleton")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}),
                                    (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "singleton")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))])})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeVariable (Core.Name "t0"))]))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabled"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#7",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "fortytwo"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "fortytwo"),
                        (Core.TermVariable (Core.Name "foo"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#8",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "fortytwo"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "fortytwo"),
                        (Core.TermVariable (Core.Name "foo"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Recursive and mutually recursive let (@wisnesky's test cases)",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "y")),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "y"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "y"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeVariable (Core.Name "t0"),
                        (Core.TypeVariable (Core.Name "t1"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
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
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "f"),
                        (Core.TermVariable (Core.Name "g"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}),
                        (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "v0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabled"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "plus"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "y"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "plus")),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "plus")),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "z"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "p0"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermProduct [
                              Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "p0"))}),
                              (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "p0"))}))])}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#6",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "z"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "z"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}),
                        (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#7",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "y"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "z"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "w"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "z")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermProduct [
                          Core.TermVariable (Core.Name "w"),
                          (Core.TermVariable (Core.Name "z"))])])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        Core.Name "t1",
                        (Core.Name "t2")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}),
                        (Core.TypeProduct [
                          Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}),
                          (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t2"))}))])])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Recursive and mutually recursive let with polymorphism",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "g"))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                            Core.applicationArgument = (Core.TermList [
                              Core.TermVariable (Core.Name "f")])})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "f"),
                        (Core.TermVariable (Core.Name "g"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "g"))}))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "f")])}))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "f"),
                        (Core.TermVariable (Core.Name "g"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "g"))}))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                              Core.applicationArgument = (Core.TermList [
                                Core.TermVariable (Core.Name "f")])}))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "f"),
                        (Core.TermVariable (Core.Name "g"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Recursion involving polymorphic functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "b"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                  Core.applicationArgument = (Core.TermList [
                                    Core.TermList [
                                      Core.TermVariable (Core.Name "x")]])})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))})))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "b"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "b"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                                Core.applicationArgument = (Core.TermList [
                                  Core.TermList [
                                    Core.TermVariable (Core.Name "x")]])}))})))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "inst"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                                Core.lambdaParameter = (Core.Name "x"),
                                Core.lambdaDomain = Nothing,
                                Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralBoolean False))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "rec"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "b0"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "f"))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "b0"))}))}))})))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "inst"),
                        (Core.TermVariable (Core.Name "rec"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeBoolean,
                        (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "inst"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "rec"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "inst"),
                        (Core.TermVariable (Core.Name "rec"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeBoolean,
                        (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "inst1"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "inst2"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                            Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "x"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))})),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "rec"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "rec")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "inst1"),
                        Core.TermVariable (Core.Name "inst2"),
                        (Core.TermVariable (Core.Name "rec"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeBoolean,
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "bar")),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "bar"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "foo")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermProduct [
                        Core.TermVariable (Core.Name "foo"),
                        (Core.TermVariable (Core.Name "bar"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeVariable (Core.Name "t0"),
                        (Core.TypeVariable (Core.Name "t1"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Literals",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralString "foo")),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralBoolean False)),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#4",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 42.0))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Pathological terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Recursion",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermVariable (Core.Name "x")),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeVariable (Core.Name "t0"))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "id"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "weird"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "id")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "id"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "id"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "weird"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "f"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "x")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "x"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t1"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "paradox"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "f"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "paradox")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "f"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "paradox"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#6",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "f"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))}))),
                          Core.letBindingType = Nothing},
                        Core.Binding {
                          Core.letBindingName = (Core.Name "g"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "g")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Infinite lists",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "self"),
                          Core.letBindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "self"))})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "self"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLet (Core.Let {
                        Core.letBindings = [
                          Core.Binding {
                            Core.letBindingName = (Core.Name "self"),
                            Core.letBindingTerm = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "self"))})),
                            Core.letBindingType = Nothing}],
                        Core.letEnvironment = (Core.TermVariable (Core.Name "self"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "self"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "e"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "self")),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "e"))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "self")),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabled"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "build"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "x"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "build")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "build")),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Polymorphism",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Simple lists and optionals",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermOptional Nothing),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0")))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambdas, lists, and products",
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
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermProduct [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermVariable (Core.Name "x"))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeProduct [
                          Core.TypeVariable (Core.Name "t0"),
                          (Core.TypeVariable (Core.Name "t0"))])}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x")])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                      (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "y"))})))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))})))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "y"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermProduct [
                            Core.TermVariable (Core.Name "y"),
                            (Core.TermVariable (Core.Name "x"))])})))}))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                          Core.functionTypeCodomain = (Core.TypeProduct [
                            Core.TypeVariable (Core.Name "t1"),
                            (Core.TypeVariable (Core.Name "t0"))])}))})))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambdas and application",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Primitives and application",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))],
                        (Core.TermList [])])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambdas and primitives",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Mixed expressions with lambdas, constants, and primitive functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Primitives",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Monomorphic primitive functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Polymorphic primitive functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "el"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermVariable (Core.Name "el")])}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "el"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermList [
                          Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                          (Core.TermVariable (Core.Name "el"))])}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "lists"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "lists"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "lists"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "lists"))}))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#6",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "list"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                          Core.applicationArgument = (Core.TermList [
                            Core.TermVariable (Core.Name "list"),
                            (Core.TermList [])])}))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#7",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "list"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                            Core.applicationArgument = (Core.TermList [
                              Core.TermVariable (Core.Name "list"),
                              (Core.TermList [])])}))}))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0"))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#8",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "lists"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "lists"))}))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Examples from the Hydra kernel",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Nested let",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "hydra.formatting.mapFirstLetter",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "mapping"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "s"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLet (Core.Let {
                          Core.letBindings = [
                            Core.Binding {
                              Core.letBindingName = (Core.Name "firstLetter"),
                              Core.letBindingTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "mapping")),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "list"))}))}))}))})),
                              Core.letBindingType = Nothing},
                            Core.Binding {
                              Core.letBindingName = (Core.Name "list"),
                              Core.letBindingTerm = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                              Core.letBindingType = Nothing}],
                          Core.letEnvironment = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "firstLetter"))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "list"))}))}))}))}))}))})))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                          Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Nominal terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Case statements",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestGraph.testTypeSimpleNumberName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "int"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "float"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})))}]})))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable TestGraph.testTypeSimpleNumberName),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationUnion (Core.CaseStatement {
                  Core.caseStatementTypeName = TestGraph.testTypeUnionMonomorphicName,
                  Core.caseStatementDefault = Nothing,
                  Core.caseStatementCases = [
                    Core.Field {
                      Core.fieldName = (Core.Name "bool"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "_"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean True))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "string"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "_"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))},
                    Core.Field {
                      Core.fieldName = (Core.Name "unit"),
                      Core.fieldTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "_"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermLiteral (Core.LiteralBoolean False))})))}]})))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable TestGraph.testTypeUnionMonomorphicName),
                    Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Projections",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Record eliminations",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
                      Core.projectionTypeName = TestGraph.testTypePersonName,
                      Core.projectionField = (Core.Name "firstName")})))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable TestGraph.testTypePersonName),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Records",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Simple records",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestGraph.testTypeLatLonName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "lat"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                        Core.Field {
                          Core.fieldName = (Core.Name "lon"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (0-122.4194))))}]})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeLatLonName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestGraph.testTypeLatLonPolyName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "lat"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                        Core.Field {
                          Core.fieldName = (Core.Name "lon"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 (0-122.4194))))}]})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeLatLonPolyName),
                        Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "lon"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestGraph.testTypeLatLonPolyName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "lat"),
                            Core.fieldTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 37.7749)))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lon"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "lon"))}]}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)),
                        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeLatLonPolyName),
                          Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "latlon"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestGraph.testTypeLatLonPolyName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "lat"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "latlon"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "lon"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "latlon"))}]}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeLatLonPolyName),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = TestGraph.testDataArthur,
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypePersonName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Record instances of simply recursive record types",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestGraph.testTypeIntListName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "head"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                        Core.Field {
                          Core.fieldName = (Core.Name "tail"),
                          Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                            Core.recordTypeName = TestGraph.testTypeIntListName,
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "head"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))},
                              Core.Field {
                                Core.fieldName = (Core.Name "tail"),
                                Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeIntListName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = TestGraph.testTypeIntListName,
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "head"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                            Core.Field {
                              Core.fieldName = (Core.Name "tail"),
                              Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                                Core.recordTypeName = TestGraph.testTypeIntListName,
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "head"),
                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "tail"),
                                    Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]}))}))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeIntListName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermRecord (Core.Record {
                      Core.recordTypeName = TestGraph.testTypeListName,
                      Core.recordFields = [
                        Core.Field {
                          Core.fieldName = (Core.Name "head"),
                          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))},
                        Core.Field {
                          Core.fieldName = (Core.Name "tail"),
                          Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                            Core.recordTypeName = TestGraph.testTypeListName,
                            Core.recordFields = [
                              Core.Field {
                                Core.fieldName = (Core.Name "head"),
                                Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 43)))},
                              Core.Field {
                                Core.fieldName = (Core.Name "tail"),
                                Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeListName),
                        Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = TestGraph.testTypeListName,
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "head"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                            Core.Field {
                              Core.fieldName = (Core.Name "tail"),
                              Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                                Core.recordTypeName = TestGraph.testTypeListName,
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "head"),
                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "tail"),
                                    Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]}))}))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeListName),
                        Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestGraph.testTypeListName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "head"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "tail"),
                            Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                              Core.recordTypeName = TestGraph.testTypeListName,
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "head"),
                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "tail"),
                                  Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeListName),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Record instances of mutually recursive record types",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermRecord (Core.Record {
                          Core.recordTypeName = TestGraph.testTypeBuddyListAName,
                          Core.recordFields = [
                            Core.Field {
                              Core.fieldName = (Core.Name "head"),
                              Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                            Core.Field {
                              Core.fieldName = (Core.Name "tail"),
                              Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                                Core.recordTypeName = TestGraph.testTypeBuddyListBName,
                                Core.recordFields = [
                                  Core.Field {
                                    Core.fieldName = (Core.Name "head"),
                                    Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                                  Core.Field {
                                    Core.fieldName = (Core.Name "tail"),
                                    Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]}))}))),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeBuddyListAName),
                        Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermRecord (Core.Record {
                        Core.recordTypeName = TestGraph.testTypeBuddyListAName,
                        Core.recordFields = [
                          Core.Field {
                            Core.fieldName = (Core.Name "head"),
                            Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                          Core.Field {
                            Core.fieldName = (Core.Name "tail"),
                            Core.fieldTerm = (Core.TermOptional (Just (Core.TermRecord (Core.Record {
                              Core.recordTypeName = TestGraph.testTypeBuddyListBName,
                              Core.recordFields = [
                                Core.Field {
                                  Core.fieldName = (Core.Name "head"),
                                  Core.fieldTerm = (Core.TermVariable (Core.Name "x"))},
                                Core.Field {
                                  Core.fieldName = (Core.Name "tail"),
                                  Core.fieldTerm = (Core.TermOptional Nothing)}]}))))}]}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeApplication (Core.ApplicationType {
                          Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeBuddyListAName),
                          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Variant terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Variants",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestGraph.testTypeTimestampName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "unixTimeMillis"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueUint64 1638200308368)))}})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeTimestampName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestGraph.testTypeUnionMonomorphicName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "string"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "bar"))}})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeUnionMonomorphicName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Polymorphic and recursive variants",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestGraph.testTypeUnionPolymorphicRecursiveName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "bool"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralBoolean True))}})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeUnionPolymorphicRecursiveName),
                        Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermUnion (Core.Injection {
                      Core.injectionTypeName = TestGraph.testTypeUnionPolymorphicRecursiveName,
                      Core.injectionField = Core.Field {
                        Core.fieldName = (Core.Name "value"),
                        Core.fieldTerm = (Core.TermLiteral (Core.LiteralString "foo"))}})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeUnionPolymorphicRecursiveName),
                        Core.applicationTypeArgument = (Core.TypeLiteral Core.LiteralTypeString)}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "other"),
                          Core.letBindingTerm = (Core.TermUnion (Core.Injection {
                            Core.injectionTypeName = TestGraph.testTypeUnionPolymorphicRecursiveName,
                            Core.injectionField = Core.Field {
                              Core.fieldName = (Core.Name "value"),
                              Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}})),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermUnion (Core.Injection {
                        Core.injectionTypeName = TestGraph.testTypeUnionPolymorphicRecursiveName,
                        Core.injectionField = Core.Field {
                          Core.fieldName = (Core.Name "other"),
                          Core.fieldTerm = (Core.TermVariable (Core.Name "other"))}}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeApplication (Core.ApplicationType {
                        Core.applicationTypeFunction = (Core.TypeVariable TestGraph.testTypeUnionPolymorphicRecursiveName),
                        Core.applicationTypeArgument = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Wrapper introductions and eliminations",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Wrapper introductions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermWrap (Core.WrappedTerm {
                      Core.wrappedTermTypeName = TestGraph.testTypeStringAliasName,
                      Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeVariable TestGraph.testTypeStringAliasName)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "v"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = TestGraph.testTypeStringAliasName,
                        Core.wrappedTermObject = (Core.TermVariable (Core.Name "v"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeVariable TestGraph.testTypeStringAliasName)}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Wrapper eliminations",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestGraph.testTypeStringAliasName))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable TestGraph.testTypeStringAliasName),
                        Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap TestGraph.testTypeStringAliasName))),
                      Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                        Core.wrappedTermTypeName = TestGraph.testTypeStringAliasName,
                        Core.wrappedTermObject = (Core.TermLiteral (Core.LiteralString "foo"))}))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []},
    Testing.TestGroup {
      Testing.testGroupName = "Simple terms",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "Application terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#1",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "x"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermVariable (Core.Name "x"))}))),
                  Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "x"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                    Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))}))),
                Testing.inferenceTestCaseOutput = Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                    Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "Function terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Lambdas",
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
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt16 137)))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List eliminations",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.foldl"))),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.add")))})),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                      Core.applicationArgument = (Core.TermList [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Optional eliminations",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.maybe"))),
                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg")))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.maybe"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg")))})),
                      Core.applicationArgument = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 137)))))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.maybe"))),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg")))})),
                      Core.applicationArgument = (Core.TermOptional Nothing)})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.maybe"))),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                          Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.pure")))})),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0"))),
                        Core.functionTypeCodomain = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#5",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.optionals.maybe"))),
                        Core.applicationArgument = (Core.TermList [])})),
                      Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "x"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermList [
                          Core.TermVariable (Core.Name "x")])})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0"))),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Tuple projections",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                      Core.tupleProjectionArity = 2,
                      Core.tupleProjectionIndex = 0,
                      Core.tupleProjectionDomain = Nothing})))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeProduct [
                          Core.TypeVariable (Core.Name "t0"),
                          (Core.TypeVariable (Core.Name "t1"))]),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermApplication (Core.Application {
                      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                        Core.tupleProjectionArity = 2,
                        Core.tupleProjectionIndex = 1,
                        Core.tupleProjectionDomain = Nothing})))),
                      Core.applicationArgument = (Core.TermProduct [
                        Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                        (Core.TermLiteral (Core.LiteralString "foo"))])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                          Core.tupleProjectionArity = 1,
                          Core.tupleProjectionIndex = 0,
                          Core.tupleProjectionDomain = Nothing})))),
                        Core.applicationArgument = (Core.TermProduct [
                          Core.TermVariable (Core.Name "x")])}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t0"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationProduct (Core.TupleProjection {
                          Core.tupleProjectionArity = 3,
                          Core.tupleProjectionIndex = 2,
                          Core.tupleProjectionDomain = Nothing})))),
                        Core.applicationArgument = (Core.TermProduct [
                          Core.TermVariable (Core.Name "x"),
                          Core.TermVariable (Core.Name "x"),
                          (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Individual terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Literal values",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralString "foo")),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeString)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralBoolean False)),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#4",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 42.0))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Let terms",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "x"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "z"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "square"),
                          Core.letBindingTerm = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "z"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.mul"))),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "z"))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "z"))}))}))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "f"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.logic.ifElse"))),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermVariable (Core.Name "square")),
                                        Core.applicationArgument = (Core.TermVariable (Core.Name "x"))}))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermVariable (Core.Name "f")),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "x"))})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "y"))}))}))})))})))})))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))})),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeBoolean),
                            Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeBoolean)}))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Optionals",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermOptional Nothing),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeOptional (Core.TypeVariable (Core.Name "t0")))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
                      (Core.TermLiteral (Core.LiteralString "foo"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Sets",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSet (S.fromList [
                      Core.TermLiteral (Core.LiteralBoolean True)])),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeSet (Core.TypeLiteral Core.LiteralTypeBoolean))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSet (S.fromList [
                      Core.TermSet (S.fromList [])])),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSet (Core.TypeSet (Core.TypeVariable (Core.Name "t0"))))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Maps",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermMap (M.fromList [
                      (Core.TermLiteral (Core.LiteralString "firstName"), (Core.TermLiteral (Core.LiteralString "Arthur"))),
                      (Core.TermLiteral (Core.LiteralString "lastName"), (Core.TermLiteral (Core.LiteralString "Dent")))])),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermMap Maps.empty),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0",
                        (Core.Name "t1")],
                      Core.typeSchemeType = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "t1"))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#3",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.Name "y"),
                        Core.lambdaDomain = Nothing,
                        Core.lambdaBody = (Core.TermMap (M.fromList [
                          (Core.TermVariable (Core.Name "x"), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.1)))),
                          (Core.TermVariable (Core.Name "y"), (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat64 0.2))))]))})))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                          Core.functionTypeCodomain = (Core.TypeMap (Core.MapType {
                            Core.mapTypeKeys = (Core.TypeVariable (Core.Name "t0")),
                            Core.mapTypeValues = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))}))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Let terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Empty let",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [],
                      Core.letEnvironment = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Trivial let",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.letBindingName = (Core.Name "foo"),
                          Core.letBindingTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))),
                          Core.letBindingType = Nothing}],
                      Core.letEnvironment = (Core.TermVariable (Core.Name "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "List terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "List of strings",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermLiteral (Core.LiteralString "bar"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List of lists of strings",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermList [
                        Core.TermLiteral (Core.LiteralString "foo")],
                      (Core.TermList [])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString)))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Empty list",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeVariable (Core.Name "t0")))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List containing an empty list",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermList [
                      Core.TermList []]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0"))))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Lambda producing a list of integers",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "List with bound variables",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "x"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermList [
                        Core.TermVariable (Core.Name "x"),
                        Core.TermLiteral (Core.LiteralString "foo"),
                        (Core.TermVariable (Core.Name "x"))])}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeList (Core.TypeLiteral Core.LiteralTypeString))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Primitive terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Monomorphic primitive functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.sub"))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                        Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                          Core.functionTypeDomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Polymorphic primitive functions",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.Name "els"),
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "els"))}))}))}))),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                        Core.functionTypeDomain = (Core.TypeList (Core.TypeList (Core.TypeVariable (Core.Name "t0")))),
                        Core.functionTypeCodomain = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))}))}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Product terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Empty product",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct []),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Non-empty monotyped products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermLiteral (Core.LiteralString "foo"),
                      (Core.TermList [
                        Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 42.0)),
                        (Core.TermLiteral (Core.LiteralFloat (Core.FloatValueFloat32 137.0)))])]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeList (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32)))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]},
            Testing.TestGroup {
              Testing.testGroupName = "Polytyped products",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermProduct [
                      Core.TermList [],
                      (Core.TermLiteral (Core.LiteralString "foo"))]),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeProduct [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0")),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = []}]}],
          Testing.testGroupCases = []},
        Testing.TestGroup {
          Testing.testGroupName = "Sum terms",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [
            Testing.TestGroup {
              Testing.testGroupName = "Singleton sum terms",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 1,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeLiteral Core.LiteralTypeString])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 1,
                      Core.sumTerm = (Core.TermList [])})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeList (Core.TypeVariable (Core.Name "t0"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]},
            Testing.TestGroup {
              Testing.testGroupName = "Non-singleton sum terms",
              Testing.testGroupDescription = Nothing,
              Testing.testGroupSubgroups = [],
              Testing.testGroupCases = [
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#1",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 0,
                      Core.sumSize = 2,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeLiteral Core.LiteralTypeString,
                        (Core.TypeVariable (Core.Name "t0"))])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]},
                Testing.TestCaseWithMetadata {
                  Testing.testCaseWithMetadataName = "#2",
                  Testing.testCaseWithMetadataCase = (Testing.TestCaseInference (Testing.InferenceTestCase {
                    Testing.inferenceTestCaseInput = (Core.TermSum (Core.Sum {
                      Core.sumIndex = 1,
                      Core.sumSize = 2,
                      Core.sumTerm = (Core.TermLiteral (Core.LiteralString "foo"))})),
                    Testing.inferenceTestCaseOutput = Core.TypeScheme {
                      Core.typeSchemeVariables = [
                        Core.Name "t0"],
                      Core.typeSchemeType = (Core.TypeSum [
                        Core.TypeVariable (Core.Name "t0"),
                        (Core.TypeLiteral Core.LiteralTypeString)])}})),
                  Testing.testCaseWithMetadataDescription = Nothing,
                  Testing.testCaseWithMetadataTags = [
                    Testing.Tag "disabledForMinimalInference"]}]}],
          Testing.testGroupCases = []}],
      Testing.testGroupCases = []}],
  Testing.testGroupCases = []}

listPrimitiveTests :: Testing.TestGroup
listPrimitiveTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.lists primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "apply",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string transformations",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.apply"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper")),
                  (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "One"),
                Core.TermLiteral (Core.LiteralString "Two"),
                (Core.TermLiteral (Core.LiteralString "Three"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "ONE"),
              Core.TermLiteral (Core.LiteralString "TWO"),
              Core.TermLiteral (Core.LiteralString "THREE"),
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "two"),
              (Core.TermLiteral (Core.LiteralString "three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty function list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.apply"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty input list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.apply"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.apply"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello")])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "HELLO")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single input",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.apply"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper")),
                  (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower")))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "Test")])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "TEST"),
              (Core.TermLiteral (Core.LiteralString "test"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "at",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.at"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "middle element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.at"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "last element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.at"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.at"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list access",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.at"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "bind",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negation function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.bind"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "arg_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "arg_"))}))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-2))),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-3))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-4))))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.bind"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "arg_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "arg_"))}))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.bind"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5))])})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "arg_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "arg_"))}))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-5)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.bind"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermFunction (Core.FunctionLambda (Core.Lambda {
                Core.lambdaParameter = (Core.Name "arg_"),
                Core.lambdaDomain = Nothing,
                Core.lambdaBody = (Core.TermApplication (Core.Application {
                  Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg"))),
                    Core.applicationArgument = (Core.TermVariable (Core.Name "arg_"))}))}))})))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-2))))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "concat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple non-empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty lists included",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                Core.TermList [],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [],
                Core.TermList [],
                (Core.TermList [])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list of lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "concat2",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two non-empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first list empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second list empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "both lists empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.concat2"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralString "a"),
                  (Core.TermLiteral (Core.LiteralString "b"))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "c"),
                (Core.TermLiteral (Core.LiteralString "d"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "b"),
              Core.TermLiteral (Core.LiteralString "c"),
              (Core.TermLiteral (Core.LiteralString "d"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "cons",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cons to non-empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cons to empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cons negative number",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "cons string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.cons"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "world")])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello"),
              (Core.TermLiteral (Core.LiteralString "world"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "drop",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop from beginning",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop zero elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop all elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop more than length",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop from empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "drop negative amount",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.drop"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "elem",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "element present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "element not present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element not present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string element present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "world"),
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "test"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string element not present",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.elem"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "missing"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "world"),
                (Core.TermLiteral (Core.LiteralString "hello"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "group",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "consecutive duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.group"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.group"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all same",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.group"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))]])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.group"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.group"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))]])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "head",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative numbers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-2))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-3))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "init",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.init"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.init"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.init"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.init"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "intercalate",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "double zero separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list of lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "lists with empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intercalate"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                (Core.TermList [])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "intersperse",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string interspersion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intersperse"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "and"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "and"),
              Core.TermLiteral (Core.LiteralString "two"),
              Core.TermLiteral (Core.LiteralString "and"),
              (Core.TermLiteral (Core.LiteralString "three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intersperse"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "only")])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "only")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intersperse"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intersperse"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "+"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "+"),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "number interspersion",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.intersperse"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "last",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative numbers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-2))),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-3))))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-3))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.last"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "length",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "many elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 8)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.length"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "map",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string to uppercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper")))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                (Core.TermLiteral (Core.LiteralString "two"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "ONE"),
              (Core.TermLiteral (Core.LiteralString "TWO"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper")))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper")))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello")])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "HELLO")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "number negation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.math.neg")))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-2))),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-3))))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "identity function",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.map"))),
                Core.applicationArgument = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.equality.identity")))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "nub",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "remove duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "no duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.nub"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "b"),
              (Core.TermLiteral (Core.LiteralString "c"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "null",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty int list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.null"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.null"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.null"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.null"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-empty string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.null"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a")])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "pure",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "one")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "number element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative number",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-5))))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-5)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "replicate",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "replicate three times",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.replicate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "replicate zero times",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.replicate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "replicate once",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.replicate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 99))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "replicate string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.replicate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello"),
              (Core.TermLiteral (Core.LiteralString "hello"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "reverse",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.reverse"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "c"),
              Core.TermLiteral (Core.LiteralString "b"),
              (Core.TermLiteral (Core.LiteralString "a"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "safeHead",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-empty int list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.safeHead"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty int list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.safeHead"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermOptional Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.safeHead"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
            Testing.evaluationTestCaseOutput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "non-empty string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.safeHead"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermOptional (Just (Core.TermLiteral (Core.LiteralString "hello"))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.safeHead"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermOptional Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "singleton",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "number element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.singleton"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "negative number",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.singleton"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "zero",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.singleton"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.singleton"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "sort",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unsorted numbers",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "already sorted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "reverse sorted",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicates",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string sort",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.sort"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "zebra"),
                Core.TermLiteral (Core.LiteralString "apple"),
                (Core.TermLiteral (Core.LiteralString "banana"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "apple"),
              Core.TermLiteral (Core.LiteralString "banana"),
              (Core.TermLiteral (Core.LiteralString "zebra"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "tail",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single element",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.tail"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "b"),
              (Core.TermLiteral (Core.LiteralString "c"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "take",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take from beginning",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take zero elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take all elements",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take more than length",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take from empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "take negative amount",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.take"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 (0-1))))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "transpose",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "square matrix",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.transpose"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.transpose"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single row",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.transpose"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single column",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.transpose"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1))],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))]])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "ragged matrix",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.transpose"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))],
                Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3))],
                (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6)))])])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))],
              Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))],
              (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 6))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "zip",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "equal length lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralString "a"))],
              Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralString "b"))],
              (Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)),
                (Core.TermLiteral (Core.LiteralString "c"))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first list shorter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralString "a"))],
              (Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralString "b"))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "second list shorter",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                (Core.TermLiteral (Core.LiteralString "a"))],
              (Core.TermProduct [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)),
                (Core.TermLiteral (Core.LiteralString "b"))])])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty first list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString "b"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty second list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [
                  Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)),
                  (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "both empty lists",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.zip"))),
                Core.applicationArgument = (Core.TermList [])})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}

primitiveTests :: Testing.TestGroup
primitiveTests = Testing.TestGroup {
  Testing.testGroupName = "Primitive functions",
  Testing.testGroupDescription = (Just "Test cases for primitive functions"),
  Testing.testGroupSubgroups = [
    listPrimitiveTests,
    stringPrimitiveTests],
  Testing.testGroupCases = []}

stringPrimitiveTests :: Testing.TestGroup
stringPrimitiveTests = Testing.TestGroup {
  Testing.testGroupName = "hydra.lib.strings primitives",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    Testing.TestGroup {
      Testing.testGroupName = "cat",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic concatenation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "onetwothree"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with empty strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString ""))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "one"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello")])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "many empty strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString ""))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "\241"),
                Core.TermLiteral (Core.LiteralString "\19990"),
                (Core.TermLiteral (Core.LiteralString "\127757"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\241\19990\127757"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "\n"),
                Core.TermLiteral (Core.LiteralString "\t"),
                (Core.TermLiteral (Core.LiteralString "\r"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numbers as strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "1"),
                Core.TermLiteral (Core.LiteralString "2"),
                (Core.TermLiteral (Core.LiteralString "3"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "123"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString " "),
                Core.TermLiteral (Core.LiteralString " "),
                (Core.TermLiteral (Core.LiteralString " "))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "   "))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed content",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "Hello"),
                Core.TermLiteral (Core.LiteralString " "),
                Core.TermLiteral (Core.LiteralString "World"),
                (Core.TermLiteral (Core.LiteralString "!"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "Hello World!"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "cat2",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic concatenation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "helloworld"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty first string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty second string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "both empty strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello "))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\241"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\19990"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\241\19990"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numeric strings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "123"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "456"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "123456"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.cat2"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\t"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\t"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "charAt",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "first character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "middle character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 2)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "last character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 4)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 111)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 97)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\241"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 241)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "space character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "newline character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "tab character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.charAt"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\t"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "fromList",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic ascii string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 101)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 111)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty code point list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 97))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "a"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 105)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "h i"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 241)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 19990)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 127757)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\241\19990\127757"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numeric characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 49)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 51)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "123"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
                Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9)),
                (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 13)))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "intercalate",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "comma separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "one,two,three"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "space separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "a"),
                Core.TermLiteral (Core.LiteralString "b"),
                (Core.TermLiteral (Core.LiteralString "c"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "abc"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multi-character separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " | "))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "A"),
                Core.TermLiteral (Core.LiteralString "B"),
                (Core.TermLiteral (Core.LiteralString "C"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "A | B | C"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single item list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "only")])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "only"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty strings in list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ","))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString "a"),
                (Core.TermLiteral (Core.LiteralString ""))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ",a,"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.intercalate"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\128279"))})),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "link1"),
                (Core.TermLiteral (Core.LiteralString "link2"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "link1\128279link2"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "length",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic word",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "string with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 11)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\241\19990\127757"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 3)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numeric string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "12345"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 5)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "long string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.length"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "this is a longer string for testing"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 35)))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "lines",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single line",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello world")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello\nworld"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello"),
              (Core.TermLiteral (Core.LiteralString "world"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one\ntwo\nthree"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "two"),
              (Core.TermLiteral (Core.LiteralString "three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "just newline",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "trailing newline",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello\n"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "leading newline",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\nhello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "hello"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple consecutive newlines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.lines"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a\n\nb"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "b"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "null",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean True))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "space only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic word",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "newline only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "tab only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.null"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\t"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralBoolean False))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "splitOn",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "double s in Mississippi",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ss"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "Mi"),
              Core.TermLiteral (Core.LiteralString "i"),
              (Core.TermLiteral (Core.LiteralString "ippi"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "whole string as separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Mississippi"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString ""))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "space separated words",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "one two three"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "two"),
              (Core.TermLiteral (Core.LiteralString "three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "leading and trailing spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " one two three "))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "two"),
              Core.TermLiteral (Core.LiteralString "three"),
              (Core.TermLiteral (Core.LiteralString ""))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple consecutive spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString " "))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  one two three"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              Core.TermLiteral (Core.LiteralString ""),
              Core.TermLiteral (Core.LiteralString "one"),
              Core.TermLiteral (Core.LiteralString "two"),
              (Core.TermLiteral (Core.LiteralString "three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "double space separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  "))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "  one two three"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "one two three"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "overlapping pattern aa in aaa",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aa"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "aaa"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "a"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "separator on empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty separator on abc",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "abc"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              Core.TermLiteral (Core.LiteralString "a"),
              Core.TermLiteral (Core.LiteralString "b"),
              (Core.TermLiteral (Core.LiteralString "c"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "both separator and string empty",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "separator not found",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "x"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello")])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "separator at start",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "h"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "ello"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "separator at end",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "o"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hell"),
              (Core.TermLiteral (Core.LiteralString ""))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multiple same separators",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "l"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "he"),
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString "o"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character string and separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString ""),
              (Core.TermLiteral (Core.LiteralString ""))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\19990"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello\19990world\19990!"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "hello"),
              Core.TermLiteral (Core.LiteralString "world"),
              (Core.TermLiteral (Core.LiteralString "!"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multi-character separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "ab"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "cabbage"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "c"),
              (Core.TermLiteral (Core.LiteralString "bage"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "newline separator",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermApplication (Core.Application {
                Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.splitOn"))),
                Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n"))})),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "line1\nline2\nline3"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralString "line1"),
              Core.TermLiteral (Core.LiteralString "line2"),
              (Core.TermLiteral (Core.LiteralString "line3"))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "toList",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single character",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 97))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic word",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 101)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 108)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 111)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with space",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "h i"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 104)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 32)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 105)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\241\19990\127757"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 241)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 19990)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 127757)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numeric string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "123"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 49)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 50)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 51)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
            Testing.evaluationTestCaseOutput = (Core.TermList [
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 10)),
              Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 9)),
              (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 13)))])})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "toLower",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed case sentence",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "One TWO threE"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "one two three"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "alphanumeric mixed case",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Abc123"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "abc123"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all uppercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all lowercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single uppercase char",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "A"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "a"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single lowercase char",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "a"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Hello World"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello world"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with punctuation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Hello, World!"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello, world!"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode accented chars",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\209\193\201\205\211\218"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\241\225\233\237\243\250"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numbers only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "12345"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "12345"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toLower"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "toUpper",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "mixed case sentence",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "One TWO threE"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "ONE TWO THREE"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "alphanumeric mixed case",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "Abc123"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "ABC123"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all lowercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all uppercase",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty string",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString ""))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single lowercase char",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "a"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "A"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single uppercase char",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "A"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "A"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with spaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello world"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO WORLD"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with punctuation",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "hello, world!"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "HELLO, WORLD!"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode accented chars",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\241\225\233\237\243\250"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\209\193\201\205\211\218"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "numbers only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "12345"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "12345"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "special characters only",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toUpper"))),
              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\t\r"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]},
    Testing.TestGroup {
      Testing.testGroupName = "unlines",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "basic two lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello\nworld\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single line",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello")])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty list",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString ""))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "three lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "one"),
                Core.TermLiteral (Core.LiteralString "two"),
                (Core.TermLiteral (Core.LiteralString "three"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "one\ntwo\nthree\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "with empty lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "hello"),
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString "world"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "hello\n\nworld\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "all empty lines",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString ""),
                Core.TermLiteral (Core.LiteralString ""),
                (Core.TermLiteral (Core.LiteralString ""))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\n\n\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "unicode content",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseEvaluation (Testing.EvaluationTestCase {
            Testing.evaluationTestCaseEvaluationStyle = Testing.EvaluationStyleEager,
            Testing.evaluationTestCaseInput = (Core.TermApplication (Core.Application {
              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.unlines"))),
              Core.applicationArgument = (Core.TermList [
                Core.TermLiteral (Core.LiteralString "\241o\241o"),
                (Core.TermLiteral (Core.LiteralString "\19990\30028"))])})),
            Testing.evaluationTestCaseOutput = (Core.TermLiteral (Core.LiteralString "\241o\241o\n\19990\30028\n"))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}],
  Testing.testGroupCases = []}
