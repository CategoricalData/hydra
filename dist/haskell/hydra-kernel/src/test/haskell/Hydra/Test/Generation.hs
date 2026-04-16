-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for code generation operations such as inferModules and inferModulesGiven

module Hydra.Test.Generation where

import qualified Hydra.Codegen as Codegen
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | Test cases for code generation operations
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "generation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "inferModulesGiven",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "incremental inference of subset matches full inference",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Packaging.termDefinitionType v0)),
                    "\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModulesGiven TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}] [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}])),
                Testing.universalTestCaseExpected = (Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Packaging.termDefinitionType v0)),
                    "\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModules TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}] [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "incremental inference of full universe matches full inference",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Packaging.termDefinitionType v0)),
                    "\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModulesGiven TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}] [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}])),
                Testing.universalTestCaseExpected = (Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Packaging.termDefinitionType v0)),
                    "\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModules TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}] [
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.a"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeType = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing}))})],
                    Packaging.moduleTermDependencies = [],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing},
                  Packaging.Module {
                    Packaging.moduleNamespace = (Packaging.Namespace "hydra.testInput.b"),
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionType = Nothing})],
                    Packaging.moduleTermDependencies = [
                      Packaging.Namespace "hydra.testInput.a"],
                    Packaging.moduleTypeDependencies = [],
                    Packaging.moduleDescription = Nothing}]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
