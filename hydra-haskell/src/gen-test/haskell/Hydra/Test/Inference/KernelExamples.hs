-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for examples from the Hydra kernel

module Hydra.Test.Inference.KernelExamples where

import qualified Hydra.Core as Core
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Examples from the Hydra kernel
allTests :: Testing.TestGroup
allTests = Testing.TestGroup {
  Testing.testGroupName = "Examples from the Hydra kernel",
  Testing.testGroupDescription = Nothing,
  Testing.testGroupSubgroups = [
    testGroupForNestedLet],
  Testing.testGroupCases = []}

testGroupForNestedLet :: Testing.TestGroup
testGroupForNestedLet = Testing.TestGroup {
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
                      Core.bindingName = (Core.Name "firstLetter"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "mapping")),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.fromList"))),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.pure"))),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.lists.head"))),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "list"))}))}))}))})),
                      Core.bindingType = Nothing},
                    Core.Binding {
                      Core.bindingName = (Core.Name "list"),
                      Core.bindingTerm = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermFunction (Core.FunctionPrimitive (Core.Name "hydra.lib.strings.toList"))),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                      Core.bindingType = Nothing}],
                  Core.letBody = (Core.TermApplication (Core.Application {
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
  Testing.testGroupCases = []}
