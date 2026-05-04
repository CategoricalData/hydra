
-- | Test cases for module and package validation (hydra.validate.packaging).
module Hydra.Sources.Test.Validate.Packaging where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Error.Packaging
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Maps       as Maps
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Packaging          as Packaging
import qualified Hydra.Dsl.Util               as Util
import qualified Hydra.Dsl.Validation         as Validation

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: Namespace
ns = Namespace "hydra.test.validate.packaging"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleDependencies = [Namespace "hydra.validate.packaging",
              Namespace "hydra.show.error.packaging"] ++ kernelTypesNamespaces,
            moduleDescription = (Just "Test cases for module and package validation")}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition checkConflictingModuleNamespacesTests,
      Phantoms.toDefinition checkConflictingVariantNamesTests,
      Phantoms.toDefinition checkDefinitionDocumentationTests,
      Phantoms.toDefinition checkDefinitionNameConventionTests,
      Phantoms.toDefinition checkDefinitionNamespacesTests,
      Phantoms.toDefinition checkDefinitionOrderingTests,
      Phantoms.toDefinition checkDuplicateDefinitionNamesTests,
      Phantoms.toDefinition checkDuplicateModuleNamespacesTests,
      Phantoms.toDefinition checkModuleNamespaceConventionTests,
      Phantoms.toDefinition checkPackageNameConventionTests,
      Phantoms.toDefinition kernelModuleTests,
      Phantoms.toDefinition kernelPackageTests,
      Phantoms.toDefinition profileBehaviourTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- ============================================================================
-- Test-data helpers
-- ============================================================================

-- | Build a Name from a String literal.
nm :: String -> TTerm Name
nm s = Core.name $ Phantoms.string s

-- | Build a Namespace from a String literal.
nsLit :: String -> TTerm Namespace
nsLit s = Packaging.namespace $ Phantoms.string s

-- | Build a PackageName from a String literal.
pn :: String -> TTerm PackageName
pn s = Packaging.packageName2 $ Phantoms.string s

-- | Build a Module with the given namespace and definitions and no dependencies.
mkModule :: String -> [TTerm Definition] -> TTerm Module
mkModule nsStr defs = Packaging.module_
  (Phantoms.just $ Phantoms.string ("Test module " <> nsStr))
  (nsLit nsStr)
  (Phantoms.list ([] :: [TTerm Namespace]))
  (Phantoms.list defs)

-- | Build a Package with the given name and modules.
mkPackage :: String -> [TTerm Module] -> TTerm Package
mkPackage nameStr mods = Packaging.package
  (pn nameStr)
  (Phantoms.list mods)
  (Phantoms.list ([] :: [TTerm PackageName]))
  (Phantoms.just $ Phantoms.string ("Test package " <> nameStr))

-- | A reified Term value (a Core.TermLiteral of a Core.LiteralString) for use
-- as a placeholder body in test fixture term-definitions.
placeholderTerm :: TTerm Term
placeholderTerm = Core.termLiteral $ Core.literalString $ Phantoms.string "value"

-- | A Term value with a description annotation at its outermost layer. Built
-- via Core.termAnnotated / Core.annotatedTerm directly so the annotation
-- survives the Haskell coder's Strip.deannotateTerm pass; the more idiomatic
-- Phantoms.doc gets stripped during code emission.
documentedPlaceholderTerm :: TTerm Term
documentedPlaceholderTerm = Core.termAnnotated $ Core.annotatedTerm
  placeholderTerm
  (Maps.fromList $ Phantoms.list [
    Phantoms.pair (Core.name $ Phantoms.string "description")
      (Core.termLiteral $ Core.literalString $ Phantoms.string "test description")])

-- | Build a TermDefinition for a fully-qualified name whose term carries a
-- top-level description annotation. Used for happy-path cases where every
-- definition needs to look documented to checkDefinitionDocumentation.
mkDocumentedTermDef :: String -> TTerm Definition
mkDocumentedTermDef fullName = Packaging.definitionTerm $ Packaging.termDefinition
  (nm fullName)
  documentedPlaceholderTerm
  (Phantoms.nothing :: TTerm (Maybe TypeScheme))

-- | Build a TermDefinition without a doc annotation (top-level term is a bare
-- literal). Used to drive checkDefinitionDocumentation failures.
mkUndocumentedTermDef :: String -> TTerm Definition
mkUndocumentedTermDef fullName = Packaging.definitionTerm $ Packaging.termDefinition
  (nm fullName)
  placeholderTerm
  (Phantoms.nothing :: TTerm (Maybe TypeScheme))

-- ============================================================================
-- Expected-value helpers
-- ============================================================================

noModuleError :: TTerm (Maybe InvalidModuleError)
noModuleError = Phantoms.nothing

justModuleError :: TTerm InvalidModuleError -> TTerm (Maybe InvalidModuleError)
justModuleError = Phantoms.just

noPackageError :: TTerm (Maybe InvalidPackageError)
noPackageError = Phantoms.nothing

justPackageError :: TTerm InvalidPackageError -> TTerm (Maybe InvalidPackageError)
justPackageError = Phantoms.just

definitionsOutOfOrderErr :: String -> String -> String -> TTerm (Maybe InvalidModuleError)
definitionsOutOfOrderErr nsStr precedingNm followingNm = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_definitionsOutOfOrder $
    Phantoms.record _DefinitionsOutOfOrderError [
      unName _DefinitionsOutOfOrderError_namespace Phantoms.>: nsLit nsStr,
      unName _DefinitionsOutOfOrderError_precedingName Phantoms.>: nm precedingNm,
      unName _DefinitionsOutOfOrderError_followingName Phantoms.>: nm followingNm]

invalidDefinitionNameErr :: String -> String -> TTerm CaseConvention -> TTerm (Maybe InvalidModuleError)
invalidDefinitionNameErr nsStr nameStr expectedConv = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_invalidDefinitionName $
    Phantoms.record _InvalidDefinitionNameError [
      unName _InvalidDefinitionNameError_namespace Phantoms.>: nsLit nsStr,
      unName _InvalidDefinitionNameError_name Phantoms.>: nm nameStr,
      unName _InvalidDefinitionNameError_expectedConvention Phantoms.>: expectedConv]

invalidNamespaceConventionErr :: String -> TTerm (Maybe InvalidModuleError)
invalidNamespaceConventionErr nsStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_invalidNamespaceConvention $
    Phantoms.record _InvalidNamespaceConventionError [
      unName _InvalidNamespaceConventionError_namespace Phantoms.>: nsLit nsStr]

missingDocumentationErr :: String -> String -> TTerm (Maybe InvalidModuleError)
missingDocumentationErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_missingDocumentation $
    Phantoms.record _MissingDocumentationError [
      unName _MissingDocumentationError_namespace Phantoms.>: nsLit nsStr,
      unName _MissingDocumentationError_name Phantoms.>: nm nameStr]

definitionNotInModuleNamespaceErr :: String -> String -> TTerm (Maybe InvalidModuleError)
definitionNotInModuleNamespaceErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_definitionNotInModuleNamespace $
    Phantoms.record _DefinitionNotInModuleNamespaceError [
      unName _DefinitionNotInModuleNamespaceError_namespace Phantoms.>: nsLit nsStr,
      unName _DefinitionNotInModuleNamespaceError_name Phantoms.>: nm nameStr]

duplicateDefinitionNameErr :: String -> String -> TTerm (Maybe InvalidModuleError)
duplicateDefinitionNameErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_duplicateDefinitionName $
    Phantoms.record _DuplicateDefinitionNameError [
      unName _DuplicateDefinitionNameError_namespace Phantoms.>: nsLit nsStr,
      unName _DuplicateDefinitionNameError_name Phantoms.>: nm nameStr]

duplicateModuleNamespaceErr :: String -> TTerm (Maybe InvalidPackageError)
duplicateModuleNamespaceErr nsStr = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_duplicateModuleNamespace $
    Phantoms.record _DuplicateModuleNamespaceError [
      unName _DuplicateModuleNamespaceError_namespace Phantoms.>: nsLit nsStr]

conflictingModuleNamespaceErr :: String -> String -> TTerm (Maybe InvalidPackageError)
conflictingModuleNamespaceErr firstNs secondNs = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_conflictingModuleNamespace $
    Phantoms.record _ConflictingModuleNamespaceError [
      unName _ConflictingModuleNamespaceError_first Phantoms.>: nsLit firstNs,
      unName _ConflictingModuleNamespaceError_second Phantoms.>: nsLit secondNs]

invalidPackageNameErr :: String -> TTerm (Maybe InvalidPackageError)
invalidPackageNameErr nameStr = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_invalidPackageName $
    Phantoms.record _InvalidPackageNameError [
      unName _InvalidPackageNameError_packageName Phantoms.>: pn nameStr]

-- ============================================================================
-- Test groups (alphabetical)
-- ============================================================================

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "All test cases for hydra.validate.packaging" $
  supergroup "validate.packaging" [
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
    kernelPackageTests,
    profileBehaviourTests]

-- | Module-level convenience: run a single module-validator over an input.
mc :: String -> TTerm (Module -> Maybe InvalidModuleError) -> TTerm Module -> TTerm (Maybe InvalidModuleError) -> TTerm TestCaseWithMetadata
mc = validatePackagingModuleCase

-- | Package-level convenience: run a single package-validator over an input.
pc :: String -> TTerm (Package -> Maybe InvalidPackageError) -> TTerm Package -> TTerm (Maybe InvalidPackageError) -> TTerm TestCaseWithMetadata
pc = validatePackagingPackageCase

checkConflictingModuleNamespacesTests :: TTermDefinition TestGroup
checkConflictingModuleNamespacesTests = define "checkConflictingModuleNamespacesTests" $
  subgroup "checkConflictingModuleNamespaces" [
    pc "empty package: no conflicts"
      checkConflictingModuleNamespacesRef
      (mkPackage "test-pkg" [])
      noPackageError,
    pc "single module: no conflicts"
      checkConflictingModuleNamespacesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" []])
      noPackageError,
    pc "distinct lowercase namespaces: no conflicts"
      checkConflictingModuleNamespacesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.bar" []])
      noPackageError,
    pc "case-insensitive collision: hydra.fooBar vs hydra.foobar"
      checkConflictingModuleNamespacesRef
      (mkPackage "test-pkg" [mkModule "hydra.fooBar" [], mkModule "hydra.foobar" []])
      (conflictingModuleNamespaceErr "hydra.fooBar" "hydra.foobar")]

checkConflictingVariantNamesTests :: TTermDefinition TestGroup
checkConflictingVariantNamesTests = define "checkConflictingVariantNamesTests" $
  subgroup "checkConflictingVariantNames" [
    -- This validator inspects union types; building union TypeDefinitions in
    -- the test DSL is verbose. The empty-module happy-path covers the
    -- baseline, and the other validators exercise the same definitionName
    -- traversal so structural coverage is high. Failing-case construction
    -- is left to integration testing.
    mc "empty module: no conflicts"
      checkConflictingVariantNamesRef
      (mkModule "hydra.foo" [])
      noModuleError]

checkDefinitionDocumentationTests :: TTermDefinition TestGroup
checkDefinitionDocumentationTests = define "checkDefinitionDocumentationTests" $
  subgroup "checkDefinitionDocumentation" [
    mc "empty module: no error"
      checkDefinitionDocumentationRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "documented term def: no error"
      checkDefinitionDocumentationRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"])
      noModuleError,
    mc "undocumented term def: error"
      checkDefinitionDocumentationRef
      (mkModule "hydra.foo" [mkUndocumentedTermDef "hydra.foo.bar"])
      (missingDocumentationErr "hydra.foo" "hydra.foo.bar")]

checkDefinitionNameConventionTests :: TTermDefinition TestGroup
checkDefinitionNameConventionTests = define "checkDefinitionNameConventionTests" $
  subgroup "checkDefinitionNameConvention" [
    mc "empty module: no error"
      checkDefinitionNameConventionRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "valid camelCase term def: no error"
      checkDefinitionNameConventionRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.someName"])
      noModuleError,
    mc "valid single-letter term def: no error"
      checkDefinitionNameConventionRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.x"])
      noModuleError,
    mc "underscore in term def name: error"
      checkDefinitionNameConventionRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bad_name"])
      (invalidDefinitionNameErr "hydra.foo" "hydra.foo.bad_name" Util.caseConventionCamel),
    mc "uppercase-first term def name: error"
      checkDefinitionNameConventionRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.BadName"])
      (invalidDefinitionNameErr "hydra.foo" "hydra.foo.BadName" Util.caseConventionCamel)]

checkDefinitionNamespacesTests :: TTermDefinition TestGroup
checkDefinitionNamespacesTests = define "checkDefinitionNamespacesTests" $
  subgroup "checkDefinitionNamespaces" [
    mc "empty module: no error"
      checkDefinitionNamespacesRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "definition with matching namespace: no error"
      checkDefinitionNamespacesRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"])
      noModuleError,
    mc "definition outside namespace: error"
      checkDefinitionNamespacesRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.baz.qux"])
      (definitionNotInModuleNamespaceErr "hydra.foo" "hydra.baz.qux")]

checkDefinitionOrderingTests :: TTermDefinition TestGroup
checkDefinitionOrderingTests = define "checkDefinitionOrderingTests" $
  subgroup "checkDefinitionOrdering" [
    mc "empty module: no error"
      checkDefinitionOrderingRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "single definition: no error"
      checkDefinitionOrderingRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.aaa"])
      noModuleError,
    mc "two definitions in order: no error"
      checkDefinitionOrderingRef
      (mkModule "hydra.foo" [
        mkDocumentedTermDef "hydra.foo.aaa",
        mkDocumentedTermDef "hydra.foo.bbb"])
      noModuleError,
    mc "two definitions out of order: error"
      checkDefinitionOrderingRef
      (mkModule "hydra.foo" [
        mkDocumentedTermDef "hydra.foo.bbb",
        mkDocumentedTermDef "hydra.foo.aaa"])
      (definitionsOutOfOrderErr "hydra.foo" "hydra.foo.bbb" "hydra.foo.aaa"),
    mc "case-sensitive ASCII order (uppercase before lowercase)"
      checkDefinitionOrderingRef
      (mkModule "hydra.foo" [
        -- ASCII: 'I' (73) < 'e' (101), so "Indented" sorts AFTER "IndentS..."
        mkDocumentedTermDef "hydra.foo.IndentedExpression",
        mkDocumentedTermDef "hydra.foo.IndentStyle"])
      (definitionsOutOfOrderErr "hydra.foo" "hydra.foo.IndentedExpression" "hydra.foo.IndentStyle")]

checkDuplicateDefinitionNamesTests :: TTermDefinition TestGroup
checkDuplicateDefinitionNamesTests = define "checkDuplicateDefinitionNamesTests" $
  subgroup "checkDuplicateDefinitionNames" [
    mc "empty module: no error"
      checkDuplicateDefinitionNamesRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "distinct names: no error"
      checkDuplicateDefinitionNamesRef
      (mkModule "hydra.foo" [
        mkDocumentedTermDef "hydra.foo.aaa",
        mkDocumentedTermDef "hydra.foo.bbb"])
      noModuleError,
    mc "duplicate names: error"
      checkDuplicateDefinitionNamesRef
      (mkModule "hydra.foo" [
        mkDocumentedTermDef "hydra.foo.aaa",
        mkDocumentedTermDef "hydra.foo.aaa"])
      (duplicateDefinitionNameErr "hydra.foo" "hydra.foo.aaa")]

checkDuplicateModuleNamespacesTests :: TTermDefinition TestGroup
checkDuplicateModuleNamespacesTests = define "checkDuplicateModuleNamespacesTests" $
  subgroup "checkDuplicateModuleNamespaces" [
    pc "empty package: no error"
      checkDuplicateModuleNamespacesRef
      (mkPackage "test-pkg" [])
      noPackageError,
    pc "distinct namespaces: no error"
      checkDuplicateModuleNamespacesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.bar" []])
      noPackageError,
    pc "duplicate namespaces: error"
      checkDuplicateModuleNamespacesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.foo" []])
      (duplicateModuleNamespaceErr "hydra.foo")]

checkModuleNamespaceConventionTests :: TTermDefinition TestGroup
checkModuleNamespaceConventionTests = define "checkModuleNamespaceConventionTests" $
  subgroup "checkModuleNamespaceConvention" [
    mc "single segment lowercase: no error"
      checkModuleNamespaceConventionRef
      (mkModule "hydra" [])
      noModuleError,
    mc "dotted lowercase: no error"
      checkModuleNamespaceConventionRef
      (mkModule "hydra.foo.bar" [])
      noModuleError,
    mc "dotted camelCase segments: no error"
      checkModuleNamespaceConventionRef
      (mkModule "hydra.test.testGraph" [])
      noModuleError,
    mc "uppercase first letter of segment: error"
      checkModuleNamespaceConventionRef
      (mkModule "hydra.Foo" [])
      (invalidNamespaceConventionErr "hydra.Foo"),
    mc "underscore in segment: error"
      checkModuleNamespaceConventionRef
      (mkModule "hydra.foo_bar" [])
      (invalidNamespaceConventionErr "hydra.foo_bar")]

checkPackageNameConventionTests :: TTermDefinition TestGroup
checkPackageNameConventionTests = define "checkPackageNameConventionTests" $
  subgroup "checkPackageNameConvention" [
    pc "single lowercase segment: no error"
      checkPackageNameConventionRef
      (mkPackage "hydra" [])
      noPackageError,
    pc "hyphen-separated lowercase: no error"
      checkPackageNameConventionRef
      (mkPackage "hydra-kernel" [])
      noPackageError,
    pc "uppercase first letter: error"
      checkPackageNameConventionRef
      (mkPackage "Hydra-kernel" [])
      (invalidPackageNameErr "Hydra-kernel"),
    pc "underscore separator: error"
      checkPackageNameConventionRef
      (mkPackage "hydra_kernel" [])
      (invalidPackageNameErr "hydra_kernel"),
    pc "dot separator: error"
      checkPackageNameConventionRef
      (mkPackage "hydra.kernel" [])
      (invalidPackageNameErr "hydra.kernel")]

-- | Smoke tests that the kernelModule orchestrator combines the per-check
-- validators and short-circuits on first failure.
kernelModuleTests :: TTermDefinition TestGroup
kernelModuleTests = define "kernelModuleTests" $
  subgroup "kernelModule (orchestrator)" [
    mc "valid module: no error"
      kernelModuleRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"])
      noModuleError,
    mc "missing documentation surfaces"
      kernelModuleRef
      (mkModule "hydra.foo" [mkUndocumentedTermDef "hydra.foo.bar"])
      (missingDocumentationErr "hydra.foo" "hydra.foo.bar"),
    mc "out-of-order definitions surface"
      kernelModuleRef
      (mkModule "hydra.foo" [
        mkDocumentedTermDef "hydra.foo.bbb",
        mkDocumentedTermDef "hydra.foo.aaa"])
      (definitionsOutOfOrderErr "hydra.foo" "hydra.foo.bbb" "hydra.foo.aaa")]

-- | Smoke tests that the kernelPackage orchestrator chains the package-level
-- checks and per-module kernelModule validation.
kernelPackageTests :: TTermDefinition TestGroup
kernelPackageTests = define "kernelPackageTests" $
  subgroup "kernelPackage (orchestrator)" [
    pc "valid package: no error"
      kernelPackageRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"]])
      noPackageError,
    pc "invalid package name surfaces first"
      kernelPackageRef
      (mkPackage "BadName" [mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"]])
      (invalidPackageNameErr "BadName"),
    pc "conflicting module namespace surfaces"
      kernelPackageRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.foo" []])
      (conflictingModuleNamespaceErr "hydra.foo" "hydra.foo")]

-- ============================================================================
-- Profile-aware behaviour tests
--
-- These exercise the post-#320 packaging validator API: an explicit
-- ValidationProfile argument and a ValidationResult return shape. The
-- legacy 'Maybe E'-shaped tests above only see the first error per pass,
-- so they can't observe multi-error accumulation, warning vs error
-- classification, or rule disabling. The cases here drive
-- validatePackagingModuleProfiledRef directly.
-- ============================================================================

-- | Build a ValidationProfile from explicit error/warning rule lists and bounds.
profileWith :: [Name] -> [Name] -> Int -> Int -> TTerm ValidationProfile
profileWith errs warns mE mW = Validation.validationProfile
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> errs)
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> warns)
  (Phantoms.int32 mE)
  (Phantoms.int32 mW)

-- | Build a ValidationResult of InvalidModuleError from explicit error and warning lists.
resultWithModule
  :: [TTerm InvalidModuleError]
  -> [TTerm InvalidModuleError]
  -> TTerm (ValidationResult InvalidModuleError)
resultWithModule errs warns = Validation.validationResult
  (Phantoms.list errs) (Phantoms.list warns)

-- | Bare InvalidModuleError for a missing-documentation finding. Differs
-- from 'missingDocumentationErr' in that it returns the bare error (not
-- Maybe), suitable for inclusion in a ValidationResult's error/warning list.
missingDocErrAt :: String -> String -> TTerm InvalidModuleError
missingDocErrAt nsStr nameStr =
  Phantoms.inject _InvalidModuleError _InvalidModuleError_missingDocumentation $
    Phantoms.record _MissingDocumentationError [
      unName _MissingDocumentationError_namespace Phantoms.>: nsLit nsStr,
      unName _MissingDocumentationError_name Phantoms.>: nm nameStr]

-- | Bare InvalidModuleError for a duplicate-definition-name finding.
duplicateDefErrAt :: String -> String -> TTerm InvalidModuleError
duplicateDefErrAt nsStr nameStr =
  Phantoms.inject _InvalidModuleError _InvalidModuleError_duplicateDefinitionName $
    Phantoms.record _DuplicateDefinitionNameError [
      unName _DuplicateDefinitionNameError_namespace Phantoms.>: nsLit nsStr,
      unName _DuplicateDefinitionNameError_name Phantoms.>: nm nameStr]

-- | Fully qualified rule names used by the profile-aware tests.
missingDocumentationRule :: Name
missingDocumentationRule = Name "hydra.error.packaging.InvalidModuleError.missingDocumentation"

duplicateDefinitionNameRule :: Name
duplicateDefinitionNameRule = Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName"

-- | An empty TTerm-encoded ValidationResult InvalidModuleError, used as the
-- starting accumulator for the orchestrator.
emptyVR :: TTerm (ValidationResult InvalidModuleError)
emptyVR = Validation.validationResult
  (Phantoms.list ([] :: [TTerm InvalidModuleError]))
  (Phantoms.list ([] :: [TTerm InvalidModuleError]))

-- | A test fixture module that violates two distinct rules simultaneously:
-- one undocumented definition (missingDocumentation) and a duplicate
-- definition name (duplicateDefinitionName). Used by the multi-error
-- accumulation cases below.
twoRuleViolationModule :: TTerm Module
twoRuleViolationModule = mkModule "hydra.foo" [
  mkUndocumentedTermDef "hydra.foo.aaa",
  mkUndocumentedTermDef "hydra.foo.aaa"]

profileBehaviourTests :: TTermDefinition TestGroup
profileBehaviourTests = define "profileBehaviourTests" $
  subgroup "profile-aware behaviour" [
    -- Multi-error accumulation: a module with two distinct rule violations
    -- (undocumented definition + duplicate definition name) produces two
    -- separate findings when maxErrors is high enough to hold both.
    -- Each per-rule check (e.g. checkDefinitionDocumentation) still
    -- short-circuits internally on the first finding it sees, so the way
    -- to produce multi-error results from the packaging orchestrator is
    -- to violate two distinct rules.
    universalCase "multi-error accumulation: two distinct rules produce two findings"
      (showValidationResultModule
        (((validatePackagingModuleProfiledRef
          Phantoms.@@ profileWith [missingDocumentationRule, duplicateDefinitionNameRule] [] 5 5)
          Phantoms.@@ emptyVR)
          Phantoms.@@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa",
         duplicateDefErrAt "hydra.foo" "hydra.foo.aaa"]
        []),
    -- Warning classification: same input, both rules live in warningRules.
    -- Expect 0 errors, 2 warnings.
    universalCase "warning classification: both rules demoted to warnings"
      (showValidationResultModule
        (((validatePackagingModuleProfiledRef
          Phantoms.@@ profileWith [] [missingDocumentationRule, duplicateDefinitionNameRule] 5 5)
          Phantoms.@@ emptyVR)
          Phantoms.@@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        []
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa",
         duplicateDefErrAt "hydra.foo" "hydra.foo.aaa"]),
    -- Rule disabling: only missingDocumentation in errorRules; the
    -- duplicate-definition-name rule is absent from both lists, so its
    -- check is skipped entirely. Expect 1 error (missing-doc), 0 findings
    -- for the duplicate.
    universalCase "rule disabling: duplicate-name rule omitted from profile"
      (showValidationResultModule
        (((validatePackagingModuleProfiledRef
          Phantoms.@@ profileWith [missingDocumentationRule] [] 5 5)
          Phantoms.@@ emptyVR)
          Phantoms.@@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa"]
        []),
    -- maxErrors bound: both rules active, but maxErrors=1. The fold
    -- iterates over guarded checks in source order; missingDocumentation
    -- fires before duplicateDefinitionName, so only the missing-doc
    -- finding is collected.
    universalCase "maxErrors bound: only first rule collected when maxErrors=1"
      (showValidationResultModule
        (((validatePackagingModuleProfiledRef
          Phantoms.@@ profileWith [missingDocumentationRule, duplicateDefinitionNameRule] [] 1 5)
          Phantoms.@@ emptyVR)
          Phantoms.@@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa"]
        [])]
