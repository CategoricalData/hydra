-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for code generation operations such as inferModules and inferModulesGiven

module Hydra.Test.Generation where
import qualified Hydra.Ast as Ast
import qualified Hydra.Codegen as Codegen
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Test.TestGraph as TestGraph
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))),
                    " = ",
                    (ShowCore.term (Packaging.termDefinitionTerm v0)),
                    "\n"]
                  Packaging.DefinitionPrimitive v0 -> Strings.cat [
                    Core.unName (Packaging.primitiveDefinitionName v0),
                    " :: <primitive>\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModulesGiven TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}] [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}])),
                Testing.universalTestCaseExpected = (\_ -> Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))),
                    " = ",
                    (ShowCore.term (Packaging.termDefinitionTerm v0)),
                    "\n"]
                  Packaging.DefinitionPrimitive v0 -> Strings.cat [
                    Core.unName (Packaging.primitiveDefinitionName v0),
                    " :: <primitive>\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModules TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}] [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "incremental inference of full universe matches full inference",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))),
                    " = ",
                    (ShowCore.term (Packaging.termDefinitionTerm v0)),
                    "\n"]
                  Packaging.DefinitionPrimitive v0 -> Strings.cat [
                    Core.unName (Packaging.primitiveDefinitionName v0),
                    " :: <primitive>\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModulesGiven TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}] [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}])),
                Testing.universalTestCaseExpected = (\_ -> Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))),
                    " = ",
                    (ShowCore.term (Packaging.termDefinitionTerm v0)),
                    "\n"]
                  Packaging.DefinitionPrimitive v0 -> Strings.cat [
                    Core.unName (Packaging.primitiveDefinitionName v0),
                    " :: <primitive>\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModules TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}] [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.a"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.a.idA"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermVariable (Core.Name "x"))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "a"],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "a")),
                            Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "a"))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.b"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.a"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.b.useId"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.a.idA")),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})),
                        Packaging.termDefinitionSignature = Nothing})]}]))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "incremental inference uses cached scheme verbatim on vacuous-quantifier universe",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Eithers.either (\e -> "<<inference error>>") (\ms -> Strings.cat (Lists.map (\m -> Strings.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType _ -> ""
                  Packaging.DefinitionTerm v0 -> Strings.cat [
                    Core.unName (Packaging.termDefinitionName v0),
                    " :: ",
                    (Maybes.maybe "<no scheme>" (\ts -> ShowCore.typeScheme ts) (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))),
                    " = ",
                    (ShowCore.term (Packaging.termDefinitionTerm v0)),
                    "\n"]
                  Packaging.DefinitionPrimitive v0 -> Strings.cat [
                    Core.unName (Packaging.primitiveDefinitionName v0),
                    " :: <primitive>\n"]) (Packaging.moduleDefinitions m))) ms)) (Codegen.inferModulesGiven TestGraph.testContext TestGraph.testGraph [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.v"),
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.v.funky"),
                        Packaging.termDefinitionTerm = (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = (Core.Name "x"),
                          Core.lambdaDomain = Nothing,
                          Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                            Core.lambdaParameter = (Core.Name "y"),
                            Core.lambdaDomain = Nothing,
                            Core.lambdaBody = (Core.TermLambda (Core.Lambda {
                              Core.lambdaParameter = (Core.Name "z"),
                              Core.lambdaDomain = Nothing,
                              Core.lambdaBody = (Core.TermVariable (Core.Name "z"))}))}))})),
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [
                            Core.Name "t0",
                            (Core.Name "t1"),
                            (Core.Name "t2")],
                          Core.typeSchemeBody = (Core.TypeFunction (Core.FunctionType {
                            Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t0")),
                            Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                              Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t1")),
                              Core.functionTypeCodomain = (Core.TypeFunction (Core.FunctionType {
                                Core.functionTypeDomain = (Core.TypeVariable (Core.Name "t2")),
                                Core.functionTypeCodomain = (Core.TypeVariable (Core.Name "t2"))}))}))})),
                          Core.typeSchemeConstraints = Nothing})))})]},
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.w"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.v"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.w.useFunky"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.v.funky")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
                        Packaging.termDefinitionSignature = Nothing})]}] [
                  Packaging.Module {
                    Packaging.moduleDescription = Nothing,
                    Packaging.moduleName = (Packaging.ModuleName "hydra.testInput.w"),
                    Packaging.moduleDependencies = [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = (Packaging.ModuleName "hydra.testInput.v"),
                        Packaging.moduleDependencyPackage = Nothing}],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "hydra.testInput.w.useFunky"),
                        Packaging.termDefinitionTerm = (Core.TermApplication (Core.Application {
                          Core.applicationFunction = (Core.TermApplication (Core.Application {
                            Core.applicationFunction = (Core.TermApplication (Core.Application {
                              Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.testInput.v.funky")),
                              Core.applicationArgument = (Core.TermLiteral (Core.LiteralString "foo"))})),
                            Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 7)))})),
                          Core.applicationArgument = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 100)))})),
                        Packaging.termDefinitionSignature = Nothing})]}])),
                Testing.universalTestCaseExpected = (\_ -> "hydra.testInput.w.useFunky :: (int32) = (hydra.testInput.v.funky\10216string\10217\10216int32\10217\10216int32\10217 @ \"foo\" @ 7:int32 @ 100:int32)\n")})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
