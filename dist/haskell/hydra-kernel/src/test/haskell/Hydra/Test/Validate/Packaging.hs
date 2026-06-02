-- Note: this is an automatically generated file. Do not edit.
-- | Test cases for module and package validation

module Hydra.Test.Validate.Packaging where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Error.Packaging as ShowErrorPackaging
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validate.Packaging as ValidatePackaging
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | All test cases for hydra.validate.packaging
allTests :: Testing.TestGroup
allTests =
    Testing.TestGroup {
      Testing.testGroupName = "validate.packaging",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [
        checkConflictingModuleNamesTests,
        checkConflictingVariantNamesTests,
        checkDefinitionDocumentationTests,
        checkDefinitionNameConventionTests,
        checkDefinitionModuleNamesTests,
        checkDefinitionOrderingTests,
        checkDuplicateDefinitionNamesTests,
        checkDuplicateModuleNamesTests,
        checkModuleNameConventionTests,
        checkPackageNameConventionTests,
        kernelModuleTests,
        kernelPackageTests,
        profileBehaviourTests],
      Testing.testGroupCases = []}
checkConflictingModuleNamesTests :: Testing.TestGroup
checkConflictingModuleNamesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkConflictingModuleNames",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty package: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single module: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct lowercase namespaces: no conflicts",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.bar"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.bar"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case-insensitive collision: hydra.fooBar vs hydra.foobar",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkConflictingModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.fooBar"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.fooBar"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foobar"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foobar"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleName (ErrorPackaging.ConflictingModuleNameError {
              ErrorPackaging.conflictingModuleNameErrorFirst = (Packaging.ModuleName "hydra.fooBar"),
              ErrorPackaging.conflictingModuleNameErrorSecond = (Packaging.ModuleName "hydra.foobar")}))))})),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkConflictingVariantNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "documented term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "undocumented term def: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionDocumentation (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
              ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.bar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDefinitionModuleNamesTests :: Testing.TestGroup
checkDefinitionModuleNamesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDefinitionModuleNames",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty module: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionModuleNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "definition with matching namespace: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionModuleNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "definition outside namespace: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionModuleNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.baz.qux"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleName (ErrorPackaging.DefinitionNotInModuleNameError {
              ErrorPackaging.definitionNotInModuleNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.definitionNotInModuleNameErrorName = (Core.Name "hydra.baz.qux")}))))})),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid camelCase term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.someName"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "valid single-letter term def: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.x"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore in term def name: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bad_name"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidDefinitionName (ErrorPackaging.InvalidDefinitionNameError {
              ErrorPackaging.invalidDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.invalidDefinitionNameErrorName = (Core.Name "hydra.foo.bad_name"),
              ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = Util.CaseConventionCamel}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase-first term def name: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.BadName"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidDefinitionName (ErrorPackaging.InvalidDefinitionNameError {
              ErrorPackaging.invalidDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.invalidDefinitionNameErrorName = (Core.Name "hydra.foo.BadName"),
              ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = Util.CaseConventionCamel}))))})),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single definition: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two definitions in order: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "two definitions out of order: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = (Core.Name "hydra.foo.bbb"),
              ErrorPackaging.definitionsOutOfOrderErrorFollowingName = (Core.Name "hydra.foo.aaa")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "case-sensitive ASCII order (uppercase before lowercase)",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDefinitionOrdering (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.IndentedExpression"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.IndentStyle"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorModuleName = (Packaging.ModuleName "hydra.foo"),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct names: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate names: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkDuplicateDefinitionNames (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
              ErrorPackaging.duplicateDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkDuplicateModuleNamesTests :: Testing.TestGroup
checkDuplicateModuleNamesTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkDuplicateModuleNames",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "empty package: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "distinct namespaces: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.bar"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.bar"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "duplicate namespaces: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkDuplicateModuleNames (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorDuplicateModuleName (ErrorPackaging.DuplicateModuleNameError {
              ErrorPackaging.duplicateModuleNameErrorModuleName = (Packaging.ModuleName "hydra.foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
checkModuleNameConventionTests :: Testing.TestGroup
checkModuleNameConventionTests =
    Testing.TestGroup {
      Testing.testGroupName = "checkModuleNameConvention",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "single segment lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dotted lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo.bar"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo.bar"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dotted camelCase segments: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.test.testGraph"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.test.testGraph"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase first letter of segment: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.Foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.Foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention (ErrorPackaging.InvalidModuleNameConventionError {
              ErrorPackaging.invalidModuleNameConventionErrorModuleName = (Packaging.ModuleName "hydra.Foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore in segment: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.checkModuleNameConvention (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo_bar"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo_bar"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention (ErrorPackaging.InvalidModuleNameConventionError {
              ErrorPackaging.invalidModuleNameConventionErrorModuleName = (Packaging.ModuleName "hydra.foo_bar")}))))})),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package hydra"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "hyphen-separated lowercase: no error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra-kernel"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package hydra-kernel"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "uppercase first letter: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "Hydra-kernel"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package Hydra-kernel"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "Hydra-kernel")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "underscore separator: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra_kernel"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package hydra_kernel"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "hydra_kernel")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "dot separator: error",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.checkPackageNameConvention (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "hydra.kernel"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package hydra.kernel"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = []}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "missing documentation surfaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing})]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
              ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.bar")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "out-of-order definitions surface",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (ValidatePackaging.kernelModule (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.bbb"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                    Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                    Core.annotatedTermAnnotation = (Maps.fromList [
                      (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                  Packaging.termDefinitionSignature = Nothing}))]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidModuleError e) (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
              ErrorPackaging.definitionsOutOfOrderErrorModuleName = (Packaging.ModuleName "hydra.foo"),
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
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = [
                    Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                      Packaging.termDefinitionMetadata = Nothing,
                      Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                        Core.annotatedTermAnnotation = (Maps.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                      Packaging.termDefinitionSignature = Nothing})]}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) Nothing)})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "invalid package name surfaces first",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "BadName"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package BadName"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = [
                    Packaging.DefinitionTerm (Packaging.TermDefinition {
                      Packaging.termDefinitionName = (Core.Name "hydra.foo.bar"),
                      Packaging.termDefinitionMetadata = Nothing,
                      Packaging.termDefinitionBody = (Core.TermAnnotated (Core.AnnotatedTerm {
                        Core.annotatedTermBody = (Core.TermLiteral (Core.LiteralString "value")),
                        Core.annotatedTermAnnotation = (Maps.fromList [
                          (Core.Name "description", (Core.TermLiteral (Core.LiteralString "test description")))])})),
                      Packaging.termDefinitionSignature = Nothing})]}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
              ErrorPackaging.invalidPackageNameErrorPackageName = (Packaging.PackageName "BadName")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "conflicting module namespace surfaces",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (ValidatePackaging.kernelPackage (Packaging.Package {
              Packaging.packageName = (Packaging.PackageName "test-pkg"),
              Packaging.packageMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test package test-pkg"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.packageDependencies = [],
              Packaging.packageModules = [
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []},
                Packaging.Module {
                  Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
                  Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                    Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                    Packaging.entityMetadataComments = [],
                    Packaging.entityMetadataSeeAlso = [],
                    Packaging.entityMetadataLifecycle = Nothing})),
                  Packaging.moduleDependencies = [],
                  Packaging.moduleDefinitions = []}]}))),
            Testing.universalTestCaseExpected = (\_ -> Maybes.maybe "valid" (\e -> ShowErrorPackaging.invalidPackageError e) (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleName (ErrorPackaging.ConflictingModuleNameError {
              ErrorPackaging.conflictingModuleNameErrorFirst = (Packaging.ModuleName "hydra.foo"),
              ErrorPackaging.conflictingModuleNameErrorSecond = (Packaging.ModuleName "hydra.foo")}))))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
profileBehaviourTests :: Testing.TestGroup
profileBehaviourTests =
    Testing.TestGroup {
      Testing.testGroupName = "profile-aware behaviour",
      Testing.testGroupDescription = Nothing,
      Testing.testGroupSubgroups = [],
      Testing.testGroupCases = [
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "multi-error accumulation: two distinct rules produce two findings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]"))),
            Testing.universalTestCaseExpected = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")}),
                (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                  ErrorPackaging.duplicateDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))],
              Validation.validationResultWarnings = []})))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")}),
                (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                  ErrorPackaging.duplicateDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))],
              Validation.validationResultWarnings = []})))) "]")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "warning classification: both rules demoted to warnings",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList []),
              Validation.validationProfileWarningRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList []),
              Validation.validationProfileWarningRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]"))),
            Testing.universalTestCaseExpected = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")}),
                (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                  ErrorPackaging.duplicateDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))]})))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")}),
                (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                  ErrorPackaging.duplicateDefinitionNameErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.duplicateDefinitionNameErrorName = (Core.Name "hydra.foo.aaa")}))]})))) "]")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "rule disabling: duplicate-name rule omitted from profile",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation"]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation"]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 5,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]"))),
            Testing.universalTestCaseExpected = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")})],
              Validation.validationResultWarnings = []})))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")})],
              Validation.validationResultWarnings = []})))) "]")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []},
        Testing.TestCaseWithMetadata {
          Testing.testCaseWithMetadataName = "maxErrors bound: only first rule collected when maxErrors=1",
          Testing.testCaseWithMetadataCase = (Testing.TestCaseUniversal (Testing.UniversalTestCase {
            Testing.universalTestCaseActual = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 1,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (ValidatePackaging.module_ (Validation.ValidationProfile {
              Validation.validationProfileErrorRules = (Sets.fromList [
                Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation",
                (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")]),
              Validation.validationProfileWarningRules = (Sets.fromList []),
              Validation.validationProfileMaxErrors = 1,
              Validation.validationProfileMaxWarnings = 5}) (Validation.ValidationResult {
              Validation.validationResultErrors = [],
              Validation.validationResultWarnings = []}) (Packaging.Module {
              Packaging.moduleName = (Packaging.ModuleName "hydra.foo"),
              Packaging.moduleMetadata = (Just (Packaging.EntityMetadata {
                Packaging.entityMetadataDescription = (Just "Test module hydra.foo"),
                Packaging.entityMetadataComments = [],
                Packaging.entityMetadataSeeAlso = [],
                Packaging.entityMetadataLifecycle = Nothing})),
              Packaging.moduleDependencies = [],
              Packaging.moduleDefinitions = [
                Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}),
                (Packaging.DefinitionTerm (Packaging.TermDefinition {
                  Packaging.termDefinitionName = (Core.Name "hydra.foo.aaa"),
                  Packaging.termDefinitionMetadata = Nothing,
                  Packaging.termDefinitionBody = (Core.TermLiteral (Core.LiteralString "value")),
                  Packaging.termDefinitionSignature = Nothing}))]}))))) "]"))),
            Testing.universalTestCaseExpected = (\_ -> Strings.cat2 (Strings.cat2 "errors=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\e -> ShowErrorPackaging.invalidModuleError e) (Validation.validationResultErrors (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")})],
              Validation.validationResultWarnings = []})))) "]")) (Strings.cat2 " warnings=[" (Strings.cat2 (Strings.intercalate ";" (Lists.map (\w -> ShowErrorPackaging.invalidModuleError w) (Validation.validationResultWarnings (Validation.ValidationResult {
              Validation.validationResultErrors = [
                ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
                  ErrorPackaging.missingDocumentationErrorModuleName = (Packaging.ModuleName "hydra.foo"),
                  ErrorPackaging.missingDocumentationErrorName = (Core.Name "hydra.foo.aaa")})],
              Validation.validationResultWarnings = []})))) "]")))})),
          Testing.testCaseWithMetadataDescription = Nothing,
          Testing.testCaseWithMetadataTags = []}]}
