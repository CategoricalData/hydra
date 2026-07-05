-- Note: this is an automatically generated file. Do not edit.

-- | Test cases for the pure module-list utilities in hydra.build.modules

module Hydra.Test.Build.Modules where

import qualified Hydra.Ast as Ast
import qualified Hydra.Build.Modules as Modules
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Overlay.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Overlay.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Test cases for the pure module-list utilities in hydra.build.modules
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "build.modules",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        Testing.TestGroup {
          Testing.testGroupName = "dedupPreservingOrder",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\s -> s) (Modules.dedupPreservingOrder [])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\s -> s) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "no duplicates unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\s -> s) (Modules.dedupPreservingOrder [
                  "a",
                  "b",
                  "c"])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\s -> s) [
                  "a",
                  "b",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "duplicates dropped, first-occurrence order kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\s -> s) (Modules.dedupPreservingOrder [
                  "b",
                  "a",
                  "b",
                  "c",
                  "a"])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\s -> s) [
                  "b",
                  "a",
                  "c"])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "filterKernelModules",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hydra.* dropped, non-hydra kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterKernelModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.core"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "hydra.core.Foo"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "example.foo"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "example.foo.Bar"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "example.foo"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "example.foo.Bar"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "hydra.json.yaml.* dropped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterKernelModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "hydra.json.yaml.model"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "hydra.json.yaml.model.X"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "other.pkg"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "other.pkg.Y"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "other.pkg"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "other.pkg.Y"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "a name merely containing hydra is kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterKernelModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "myhydra.x"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "myhydra.x.Z"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "myhydra.x"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "myhydra.x.Z"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty input",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterKernelModules [])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "filterTypeModules",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type-def module kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterTypeModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "term-only module dropped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterTypeModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty module dropped",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterTypeModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.empty"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = []}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed module (type + term) kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) (Modules.filterTypeModules [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Packaging.unModuleName (Packaging.moduleName m)) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "secondLevelDir",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "two segments plus file",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "hydra/java/coder.json")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) (Just "hydra/java"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "deep path keeps only first two",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "hydra/python/model/core.json")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) (Just "hydra/python"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "exactly two segments",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "hydra/java")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) (Just "hydra/java"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "one segment is nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "hydra")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty string is nothing",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) Nothing)})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "trailing separator kept (Python semantics)",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.optional (\s -> s) (Modules.secondLevelDir "hydra/")),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.optional (\s -> s) (Just "hydra/"))})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "stripAllTermTypes",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "empty list",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName m),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions m))]) (Modules.stripAllTermTypes [])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName m),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions m))]) [])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "each module's term signatures cleared",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> ShowCore.list (\m -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName m),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions m))]) (Modules.stripAllTermTypes [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "b.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "b.mixed.g"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "b.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}])),
                Testing.universalTestCaseExpected = (\_ -> ShowCore.list (\m -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName m),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions m))]) [
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]},
                  Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "b.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "b.mixed.g"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "b.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]},
        Testing.TestGroup {
          Testing.testGroupName = "stripTermTypes",
          Testing.testGroupDescription = Nothing,
          Testing.testGroupSubgroups = [],
          Testing.testGroupCases = [
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "term signature cleared, body preserved",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]}))),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]}))))]),
                Testing.universalTestCaseExpected = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]})),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.terms"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.terms.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))})]})))])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "type definition passes through unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}))),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]}))))]),
                Testing.universalTestCaseExpected = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]})),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.types"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.types.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}})]})))])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "mixed module: term stripped, type kept",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}))),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}))),
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]}))))]),
                Testing.universalTestCaseExpected = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]})),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.mixed"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionTerm (Packaging.TermDefinition {
                        Packaging.termDefinitionName = (Core.Name "a.mixed.f"),
                        Packaging.termDefinitionMetadata = Nothing,
                        Packaging.termDefinitionSignature = Nothing,
                        Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 42)))}),
                      (Packaging.DefinitionType (Packaging.TypeDefinition {
                        Packaging.typeDefinitionName = (Core.Name "a.mixed.T"),
                        Packaging.typeDefinitionMetadata = Nothing,
                        Packaging.typeDefinitionBody = Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing}}))]})))])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []},
            Testing.TestCaseWithMetadata {
              Testing.testCaseWithMetadataName = "primitive definition passes through unchanged",
              Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
                Testing.universalTestCaseActual = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.prims"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionPrimitive (Packaging.PrimitiveDefinition {
                        Packaging.primitiveDefinitionName = (Core.Name "a.prims.p"),
                        Packaging.primitiveDefinitionMetadata = Nothing,
                        Packaging.primitiveDefinitionSignature = (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing})),
                        Packaging.primitiveDefinitionIsPure = True,
                        Packaging.primitiveDefinitionIsTotal = True,
                        Packaging.primitiveDefinitionDefaultImplementation = Nothing})]}))),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Modules.stripTermTypes (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.prims"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionPrimitive (Packaging.PrimitiveDefinition {
                        Packaging.primitiveDefinitionName = (Core.Name "a.prims.p"),
                        Packaging.primitiveDefinitionMetadata = Nothing,
                        Packaging.primitiveDefinitionSignature = (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing})),
                        Packaging.primitiveDefinitionIsPure = True,
                        Packaging.primitiveDefinitionIsTotal = True,
                        Packaging.primitiveDefinitionDefaultImplementation = Nothing})]}))))]),
                Testing.universalTestCaseExpected = (\_ -> Strings.cat [
                  Packaging.unModuleName (Packaging.moduleName (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.prims"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionPrimitive (Packaging.PrimitiveDefinition {
                        Packaging.primitiveDefinitionName = (Core.Name "a.prims.p"),
                        Packaging.primitiveDefinitionMetadata = Nothing,
                        Packaging.primitiveDefinitionSignature = (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing})),
                        Packaging.primitiveDefinitionIsPure = True,
                        Packaging.primitiveDefinitionIsTotal = True,
                        Packaging.primitiveDefinitionDefaultImplementation = Nothing})]})),
                  "|",
                  (ShowCore.list (\d -> case d of
                    Packaging.DefinitionType v0 -> Strings.cat [
                      "type:",
                      (Core.unName (Packaging.typeDefinitionName v0))]
                    Packaging.DefinitionPrimitive v0 -> Strings.cat [
                      "prim:",
                      (Core.unName (Packaging.primitiveDefinitionName v0))]
                    Packaging.DefinitionTerm v0 -> Strings.cat [
                      "term:",
                      (Core.unName (Packaging.termDefinitionName v0)),
                      ":sig=",
                      (Optionals.cases (Packaging.termDefinitionSignature v0) "no" (\_2 -> "yes"))]) (Packaging.moduleDefinitions (Packaging.Module {
                    Packaging.moduleName = (Packaging.ModuleName "a.prims"),
                    Packaging.moduleMetadata = Nothing,
                    Packaging.moduleDependencies = [],
                    Packaging.moduleDefinitions = [
                      Packaging.DefinitionPrimitive (Packaging.PrimitiveDefinition {
                        Packaging.primitiveDefinitionName = (Core.Name "a.prims.p"),
                        Packaging.primitiveDefinitionMetadata = Nothing,
                        Packaging.primitiveDefinitionSignature = (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32)),
                          Core.typeSchemeConstraints = Nothing})),
                        Packaging.primitiveDefinitionIsPure = True,
                        Packaging.primitiveDefinitionIsTotal = True,
                        Packaging.primitiveDefinitionDefaultImplementation = Nothing})]})))])})),
              Testing.testCaseWithMetadataDescription = Nothing,
              Testing.testCaseWithMetadataTags = []}]}],
      Testing.testGroupCases = []}
