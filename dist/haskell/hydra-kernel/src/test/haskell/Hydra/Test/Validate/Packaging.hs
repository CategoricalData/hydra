-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for module and package validation

module Hydra.Test.Validate.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Show.Error.Packaging as ShowErrorPackaging
import qualified Hydra.Testing as Testing
import qualified Hydra.Util as Util
import qualified Hydra.Validate.Packaging as ValidatePackaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | All test cases for hydra.validate.packaging
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "validate.packaging",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        checkConflictingModuleNamespacesTests,
        checkConflictingVariantNamesTests,
        checkDefinitionDocumentationTests,
        checkDefinitionNameConventionTests,
        checkDefinitionNamespacesTests,
        checkDefinitionOrderingTests,
        checkDuplicateDefinitionNamesTests,
        checkDuplicateModuleNamespacesTests,
        checkModuleNamespaceConventionTests,
        checkPackageNameConventionTests,
        kernelModuleTests,
        kernelPackageTests],
      Testing.testGroupCases = []}
checkConflictingModuleNamespacesTests :: Testing.TestGroup
checkConflictingModuleNamespacesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkConflictingModuleNamespaces",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty package: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single module: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct lowercase namespaces: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.bar"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.bar"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case-insensitive collision: hydra.fooBar vs hydra.foobar",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.fooBar"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.fooBar"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foobar"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foobar"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleNamespace (ErrorPackaging.ConflictingModuleNamespaceError {
              ErrorPackaging.conflictingModuleNamespaceErrorFirst = (Packaging.Namespace "hydra.fooBar"),
              ErrorPackaging.conflictingModuleNamespaceErrorSecond = (Packaging.Namespace "hydra.foobar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkConflictingVariantNamesTests :: Testing.TestGroup
checkConflictingVariantNamesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkConflictingVariantNames",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkConflictingVariantNames (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDefinitionDocumentationTests :: Testing.TestGroup
checkDefinitionDocumentationTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDefinitionDocumentation",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "documented term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "undocumented term def: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionTerm = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
              ErrorPackaging.missingDocumentationErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.bar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDefinitionNameConventionTests :: Testing.TestGroup
checkDefinitionNameConventionTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDefinitionNameConvention",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid camelCase term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.someName"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid single-letter term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.x"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore in term def name: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bad_name"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidDefinitionName (ErrorPackaging.InvalidDefinitionNameError {
              ErrorPackaging.invalidDefinitionNameErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.invalidDefinitionNameErrorName = (Core.Name "hydra.foo.bad_name"),
              ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = Util.CaseConventionCamel}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase-first term def name: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.BadName"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidDefinitionName (ErrorPackaging.InvalidDefinitionNameError {
              ErrorPackaging.invalidDefinitionNameErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.invalidDefinitionNameErrorName = (Core.Name "hydra.foo.BadName"),
              ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = Util.CaseConventionCamel}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDefinitionNamespacesTests :: Testing.TestGroup
checkDefinitionNamespacesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDefinitionNamespaces",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNamespaces (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "definition with matching namespace: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNamespaces (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "definition outside namespace: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNamespaces (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.baz.qux"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleNamespace (ErrorPackaging.DefinitionNotInModuleNamespaceError {
              ErrorPackaging.definitionNotInModuleNamespaceErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.definitionNotInModuleNamespaceErrorName = (Core.Name "hydra.baz.qux")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDefinitionOrderingTests :: Testing.TestGroup
checkDefinitionOrderingTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDefinitionOrdering",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single definition: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two definitions in order: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two definitions out of order: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = (Core.Name "hydra.foo.bbb"),
              ErrorPackaging.definitionsOutOfOrderErrorFollowingName = (Core.Name "hydra.foo.aaa")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case-sensitive ASCII order (uppercase before lowercase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.IndentedExpression"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.IndentStyle"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = (Core.Name "hydra.foo.IndentedExpression"),
              ErrorPackaging.definitionsOutOfOrderErrorFollowingName = (Core.Name "hydra.foo.IndentStyle")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDuplicateDefinitionNamesTests :: Testing.TestGroup
checkDuplicateDefinitionNamesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDuplicateDefinitionNames",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct names: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate names: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
              ErrorPackaging.duplicateDefinitionNameErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDuplicateModuleNamespacesTests :: Testing.TestGroup
checkDuplicateModuleNamespacesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDuplicateModuleNamespaces",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty package: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct namespaces: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.bar"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.bar"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate namespaces: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNamespaces (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorDuplicateModuleNamespace (ErrorPackaging.DuplicateModuleNamespaceError {
              ErrorPackaging.duplicateModuleNamespaceErrorNamespace = (Packaging.Namespace "hydra.foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkModuleNamespaceConventionTests :: Testing.TestGroup
checkModuleNamespaceConventionTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkModuleNamespaceConvention",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single segment lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNamespaceConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dotted lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNamespaceConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo.bar"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo.bar"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dotted camelCase segments: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNamespaceConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.test.testGraph"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.test.testGraph"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase first letter of segment: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNamespaceConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.Foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.Foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidNamespaceConvention (ErrorPackaging.InvalidNamespaceConventionError {
              ErrorPackaging.invalidNamespaceConventionErrorNamespace = (Packaging.Namespace "hydra.Foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore in segment: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNamespaceConvention (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo_bar"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo_bar"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidNamespaceConvention (ErrorPackaging.InvalidNamespaceConventionError {
              ErrorPackaging.invalidNamespaceConventionErrorNamespace = (Packaging.Namespace "hydra.foo_bar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkPackageNameConventionTests :: Testing.TestGroup
checkPackageNameConventionTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkPackageNameConvention",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single lowercase segment: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package hydra")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hyphen-separated lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra-kernel"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package hydra-kernel")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase first letter: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "Hydra-kernel"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package Hydra-kernel")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "Hydra-kernel")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore separator: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra_kernel"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package hydra_kernel")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "hydra_kernel")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dot separator: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra.kernel"),
              Packaging.packageModules = [],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package hydra.kernel")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "hydra.kernel")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
kernelModuleTests :: Testing.TestGroup
kernelModuleTests =
    Testing.TestGroup {
      Testing.testGroupName = "kernelModule (orchestrator)",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "missing documentation surfaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionTerm = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionTypeScheme = Nothing})]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
              ErrorPackaging.missingDocumentationErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.bar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "out-of-order definitions surface",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleDescription = (Just "Test module hydra.foo"),
              Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionTypeScheme = Nothing}))]}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorNamespace = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = (Core.Name "hydra.foo.bbb"),
              ErrorPackaging.definitionsOutOfOrderErrorFollowingName = (Core.Name "hydra.foo.aaa")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
kernelPackageTests :: Testing.TestGroup
kernelPackageTests =
    Testing.TestGroup {
      Testing.testGroupName = "kernelPackage (orchestrator)",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid package: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = [
                    Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                      Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                        Core.annotatedTermAnnotation = (Maps.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                      Packaging.termDefinitionTypeScheme = Nothing})]}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid package name surfaces first",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "BadName"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = [
                    Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                      Packaging.termDefinitionTerm = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                        Core.annotatedTermAnnotation = (Maps.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                      Packaging.termDefinitionTypeScheme = Nothing})]}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package BadName")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "BadName")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "conflicting module namespace surfaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleDescription = (Just "Test module hydra.foo"),
                  Packaging.moduleNamespace = (Packaging.Namespace "hydra.foo"),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}],
              Packaging.packageDependencies = [],
              Packaging.packageDescription = (Just "Test package test-pkg")}))),
            Testing.universalTestCaseExpected = (Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleNamespace (ErrorPackaging.ConflictingModuleNamespaceError {
              ErrorPackaging.conflictingModuleNamespaceErrorFirst = (Packaging.Namespace "hydra.foo"),
              ErrorPackaging.conflictingModuleNamespaceErrorSecond = (Packaging.Namespace "hydra.foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
