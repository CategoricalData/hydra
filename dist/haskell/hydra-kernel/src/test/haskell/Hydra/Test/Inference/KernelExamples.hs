-- Note: this is an automatically generated file. Do not edit.

-- | Inference tests for examples from the Hydra kernel

module Hydra.Test.Inference.KernelExamples where

import qualified Hydra.Core as Core
import qualified Hydra.Inference as Inference
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Examples from the Hydra kernel
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "Examples from the Hydra kernel",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        testGroupForNestedLet],
      Testing.testGroupCases = []}

testGroupForNestedLet :: Testing.TestGroup
testGroupForNestedLet =
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
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "mapping"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                    Core.lambdaParameter = (Core.Name "s"),
                    Core.lambdaDomain = Nothing,
                    Core.lambdaBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "list"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.toList")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                          Core.bindingType = Nothing},
                        Core.Binding {
                          Core.bindingName = (Core.Name "firstLetter"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "mapping")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.fromList")),
                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maybes.cat")),
                                Core.applicationArgument = (Core.TermList [
                                  Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.maybeHead")),
                                    Core.applicationArgument = (Core.TermVariable (Core.Name "list"))})])}))}))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.null")),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                          Core.applicationArgument = (Core.TermVariable (Core.Name "s"))})),
                        Core.applicationArgument = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "firstLetter"))})),
                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.fromList")),
                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.lists.drop")),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "list"))}))}))}))}))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)})),
                    Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                      Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                      Core.functionTypeCodomain = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Recursive let with pair return (ifElse)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#2",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "input"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "go"),
                        Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "depth"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "subst"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "s"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.logic.ifElse")),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.null")),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))})),
                                  Core.applicationArgument = (Core.TermPair (Core.TermVariable (Core.Name "subst"), (Core.TermVariable (Core.Name "s"))))})),
                                Core.applicationArgument = (Core.TermApplication (Core.Application {
                                  Core.applicationFunction = (Core.TermApplication (Core.Application {
                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermVariable (Core.Name "go")),
                                      Core.applicationArgument = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                                          Core.applicationArgument = (Core.TermVariable (Core.Name "depth"))})),
                                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                      Core.applicationFunction = (Core.TermApplication (Core.Application {
                                        Core.applicationFunction = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.insert")),
                                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "key"))})),
                                        Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "val"))})),
                                      Core.applicationArgument = (Core.TermVariable (Core.Name "subst"))}))})),
                                  Core.applicationArgument = (Core.TermVariable (Core.Name "s"))}))}))}))}))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "result"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "go")),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.maps.empty"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "input"))})),
                          Core.bindingType = Nothing},
                        Core.Binding {
                          Core.bindingName = (Core.Name "subst"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "result"))})),
                          Core.bindingType = Nothing},
                        Core.Binding {
                          Core.bindingName = (Core.Name "body"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "result"))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermPair (Core.TermVariable (Core.Name "subst"), (Core.TermVariable (Core.Name "body"))))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeLiteral Core.LiteralTypeString),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeLiteral Core.LiteralTypeString),
                        Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)})),
                      Core.pairTypeSecond = (Core.TypeLiteral Core.LiteralTypeString)}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference"]}]},
        Testing.TestGroup {
          Testing.testGroupName = "Recursive let with pair return (case on Type)",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "#3",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> Strings.cat2 "INFERENCE ERROR: " "failed") (\result -> ShowCore.typeScheme (Pairs.second (Pairs.first result))) (Inference.inferTypeOf TestGraph.testContext TestGraph.testGraph (Core.TermLambda (Core.Lambda {
                  Core.lambdaParameter = (Core.Name "typ"),
                  Core.lambdaDomain = Nothing,
                  Core.lambdaBody = (Core.TermLet (Core.Let {
                    Core.letBindings = [
                      Core.Binding {
                        Core.bindingName = (Core.Name "go"),
                        Core.bindingTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "depth"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "subst"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "t"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermCases (Core.CaseStatement {
                                  Core.caseStatementTypeName = (Core.Name "hydra.core.Type"),
                                  Core.caseStatementDefault = (Just (Core.TermPair (Core.TermVariable (Core.Name "subst"), (Core.TermVariable (Core.Name "t"))))),
                                  Core.caseStatementCases = [
                                    Core.Field {
                                      Core.fieldName = (Core.Name "forall"),
                                      Core.fieldTerm = (Core.TermLambda (Core.Lambda {
                                        Core.lambdaParameter = (Core.Name "ft"),
                                        Core.lambdaDomain = Nothing,
                                        Core.lambdaBody = (Core.TermApplication (Core.Application {
                                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermVariable (Core.Name "go")),
                                              Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.math.add")),
                                                  Core.applicationArgument = (Core.TermVariable (Core.Name "depth"))})),
                                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 1)))}))})),
                                            Core.applicationArgument = (Core.TermApplication (Core.Application {
                                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                  Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.maps.insert")),
                                                  Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermProject (Core.Projection {
                                                      Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                                      Core.projectionField = (Core.Name "parameter")})),
                                                    Core.applicationArgument = (Core.TermVariable (Core.Name "ft"))}))})),
                                                Core.applicationArgument = (Core.TermWrap (Core.WrappedTerm {
                                                  Core.wrappedTermTypeName = (Core.Name "hydra.core.Name"),
                                                  Core.wrappedTermBody = (Core.TermApplication (Core.Application {
                                                    Core.applicationFunction = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.strings.cat2")),
                                                      Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "_"))})),
                                                    Core.applicationArgument = (Core.TermApplication (Core.Application {
                                                      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
                                                      Core.applicationArgument = (Core.TermVariable (Core.Name "depth"))}))}))}))})),
                                              Core.applicationArgument = (Core.TermVariable (Core.Name "subst"))}))})),
                                          Core.applicationArgument = (Core.TermApplication (Core.Application {
                                            Core.applicationFunction = (Core.TermProject (Core.Projection {
                                              Core.projectionTypeName = (Core.Name "hydra.core.ForallType"),
                                              Core.projectionField = (Core.Name "body")})),
                                            Core.applicationArgument = (Core.TermVariable (Core.Name "ft"))}))}))}))}]})),
                                Core.applicationArgument = (Core.TermVariable (Core.Name "t"))}))}))}))})),
                        Core.bindingType = Nothing}],
                    Core.letBody = (Core.TermLet (Core.Let {
                      Core.letBindings = [
                        Core.Binding {
                          Core.bindingName = (Core.Name "result"),
                          Core.bindingTerm = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermApplication (Core.Application {
                                Core.applicationFunction = (Core.TermVariable (Core.Name "go")),
                                Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 0)))})),
                              Core.applicationArgument = (Core.TermVariable (Core.Name "hydra.lib.maps.empty"))})),
                            Core.applicationArgument = (Core.TermVariable (Core.Name "typ"))})),
                          Core.bindingType = Nothing}],
                      Core.letBody = (Core.TermPair (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.first")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "result"))}), (Core.TermApplication (Core.Application {
                        Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.pairs.second")),
                        Core.applicationArgument = (Core.TermVariable (Core.Name "result"))}))))}))}))})))),
                Testing.universalTestCaseExpected = (ShowCore.typeScheme (Core.TypeScheme {
                  Core.typeSchemeVariables = [],
                  Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                    Core.functionTypeDomain = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                    Core.functionTypeCodomain = (Core.TypePair (Core.PairType {
                      Core.pairTypeFirst = (Core.TypeMap (Core.MapType {
                        Core.mapTypeKeys = (Core.TypeVariable (Core.Name "hydra.core.Name")),
                        Core.mapTypeValues = (Core.TypeVariable (Core.Name "hydra.core.Name"))})),
                      Core.pairTypeSecond = (Core.TypeVariable (Core.Name "hydra.core.Type"))}))})),
                  Core.typeSchemeConstraints = Nothing}))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = [
                Testing.Tag "disabledForMinimalInference",
                (Testing.Tag "disabled")]}]}],
      Testing.testGroupCases = []}
