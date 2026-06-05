
-- | Test cases for module and package validation (hydra.validate.packaging).
module Hydra.Sources.Test.Validate.Packaging where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Error.Packaging
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Maps       as Maps
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import           Hydra.Dsl.Meta.Phantoms                ((@@))
import qualified Hydra.Dsl.Packaging          as Packaging
import qualified Hydra.Dsl.Util               as Util
import qualified Hydra.Dsl.Validation         as Validation
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Data.Map as M

import Hydra.Testing
import Hydra.Sources.Libraries


ns :: ModuleName
ns = ModuleName "hydra.test.validate.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.validate.packaging",
              ModuleName "hydra.show.error.packaging"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata ((Just "Test cases for module and package validation"))}
  where
    definitions = [
      Phantoms.toDefinition allTests,
      Phantoms.toDefinition checkConflictingModuleNamesTests,
      Phantoms.toDefinition checkConflictingVariantNamesTests,
      Phantoms.toDefinition checkDefinitionDocumentationTests,
      Phantoms.toDefinition checkDefinitionNameConventionTests,
      Phantoms.toDefinition checkDefinitionModuleNamesTests,
      Phantoms.toDefinition checkDefinitionOrderingTests,
      Phantoms.toDefinition checkDuplicateDefinitionNamesTests,
      Phantoms.toDefinition checkDuplicateModuleNamesTests,
      Phantoms.toDefinition checkModuleNameConventionTests,
      Phantoms.toDefinition checkPackageNameConventionTests,
      Phantoms.toDefinition kernelModuleTests,
      Phantoms.toDefinition kernelPackageTests,
      Phantoms.toDefinition profileBehaviourTests]

define :: String -> TypedTerm a -> TypedTermDefinition a
define = definitionInModule module_

-- ============================================================================
-- Test-data helpers
-- ============================================================================

allTests :: TypedTermDefinition TestGroup
allTests = define "allTests" $
  Phantoms.doc "All test cases for hydra.validate.packaging" $
  supergroup "validate.packaging" [
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
    profileBehaviourTests]

checkConflictingModuleNamesTests :: TypedTermDefinition TestGroup
checkConflictingModuleNamesTests = define "checkConflictingModuleNamesTests" $
  subgroup "checkConflictingModuleNames" [
    pc "empty package: no conflicts"
      checkConflictingModuleNamesRef
      (mkPackage "test-pkg" [])
      noPackageError,
    pc "single module: no conflicts"
      checkConflictingModuleNamesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" []])
      noPackageError,
    pc "distinct lowercase namespaces: no conflicts"
      checkConflictingModuleNamesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.bar" []])
      noPackageError,
    pc "case-insensitive collision: hydra.fooBar vs hydra.foobar"
      checkConflictingModuleNamesRef
      (mkPackage "test-pkg" [mkModule "hydra.fooBar" [], mkModule "hydra.foobar" []])
      (conflictingModuleNameErr "hydra.fooBar" "hydra.foobar")]

checkConflictingVariantNamesTests :: TypedTermDefinition TestGroup
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

checkDefinitionDocumentationTests :: TypedTermDefinition TestGroup
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

checkDefinitionModuleNamesTests :: TypedTermDefinition TestGroup
checkDefinitionModuleNamesTests = define "checkDefinitionModuleNamesTests" $
  subgroup "checkDefinitionModuleNames" [
    mc "empty module: no error"
      checkDefinitionModuleNamesRef
      (mkModule "hydra.foo" [])
      noModuleError,
    mc "definition with matching namespace: no error"
      checkDefinitionModuleNamesRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.foo.bar"])
      noModuleError,
    mc "definition outside namespace: error"
      checkDefinitionModuleNamesRef
      (mkModule "hydra.foo" [mkDocumentedTermDef "hydra.baz.qux"])
      (definitionNotInModuleNameErr "hydra.foo" "hydra.baz.qux")]

checkDefinitionNameConventionTests :: TypedTermDefinition TestGroup
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

checkDefinitionOrderingTests :: TypedTermDefinition TestGroup
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

checkDuplicateDefinitionNamesTests :: TypedTermDefinition TestGroup
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

checkDuplicateModuleNamesTests :: TypedTermDefinition TestGroup
checkDuplicateModuleNamesTests = define "checkDuplicateModuleNamesTests" $
  subgroup "checkDuplicateModuleNames" [
    pc "empty package: no error"
      checkDuplicateModuleNamesRef
      (mkPackage "test-pkg" [])
      noPackageError,
    pc "distinct namespaces: no error"
      checkDuplicateModuleNamesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.bar" []])
      noPackageError,
    pc "duplicate namespaces: error"
      checkDuplicateModuleNamesRef
      (mkPackage "test-pkg" [mkModule "hydra.foo" [], mkModule "hydra.foo" []])
      (duplicateModuleNameErr "hydra.foo")]

checkModuleNameConventionTests :: TypedTermDefinition TestGroup
checkModuleNameConventionTests = define "checkModuleNameConventionTests" $
  subgroup "checkModuleNameConvention" [
    mc "single segment lowercase: no error"
      checkModuleNameConventionRef
      (mkModule "hydra" [])
      noModuleError,
    mc "dotted lowercase: no error"
      checkModuleNameConventionRef
      (mkModule "hydra.foo.bar" [])
      noModuleError,
    mc "dotted camelCase segments: no error"
      checkModuleNameConventionRef
      (mkModule "hydra.test.testGraph" [])
      noModuleError,
    mc "uppercase first letter of segment: error"
      checkModuleNameConventionRef
      (mkModule "hydra.Foo" [])
      (invalidModuleNameConventionErr "hydra.Foo"),
    mc "underscore in segment: error"
      checkModuleNameConventionRef
      (mkModule "hydra.foo_bar" [])
      (invalidModuleNameConventionErr "hydra.foo_bar")]

checkPackageNameConventionTests :: TypedTermDefinition TestGroup
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

conflictingModuleNameErr :: String -> String -> TypedTerm (Maybe InvalidPackageError)
conflictingModuleNameErr firstNs secondNs = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_conflictingModuleName $
    Phantoms.record _ConflictingModuleNameError [
      unName _ConflictingModuleNameError_first Phantoms.>: nsLit firstNs,
      unName _ConflictingModuleNameError_second Phantoms.>: nsLit secondNs]

definitionNotInModuleNameErr :: String -> String -> TypedTerm (Maybe InvalidModuleError)
definitionNotInModuleNameErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_definitionNotInModuleName $
    Phantoms.record _DefinitionNotInModuleNameError [
      unName _DefinitionNotInModuleNameError_moduleName Phantoms.>: nsLit nsStr,
      unName _DefinitionNotInModuleNameError_name Phantoms.>: nm nameStr]

definitionsOutOfOrderErr :: String -> String -> String -> TypedTerm (Maybe InvalidModuleError)
definitionsOutOfOrderErr nsStr precedingNm followingNm = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_definitionsOutOfOrder $
    Phantoms.record _DefinitionsOutOfOrderError [
      unName _DefinitionsOutOfOrderError_moduleName Phantoms.>: nsLit nsStr,
      unName _DefinitionsOutOfOrderError_precedingName Phantoms.>: nm precedingNm,
      unName _DefinitionsOutOfOrderError_followingName Phantoms.>: nm followingNm]

-- | A Term value with a description annotation at its outermost layer. Built
-- via Core.termAnnotated / Core.annotatedTerm directly so the annotation
-- survives the Haskell coder's Strip.deannotateTerm pass; the more idiomatic
-- Phantoms.doc gets stripped during code emission.
documentedPlaceholderTerm :: TypedTerm Term
documentedPlaceholderTerm = Core.termAnnotated $ Core.annotatedTerm
  placeholderTerm
  (Annotations.wrapAnnotationMap @@ (Maps.fromList (Phantoms.list [
    Phantoms.pair (Core.name $ Phantoms.string "description")
      (Core.termLiteral $ Core.literalString $ Phantoms.string "test description")])))

-- | Bare InvalidModuleError for a duplicate-definition-name finding.
duplicateDefErrAt :: String -> String -> TypedTerm InvalidModuleError
duplicateDefErrAt nsStr nameStr =
  Phantoms.inject _InvalidModuleError _InvalidModuleError_duplicateDefinitionName $
    Phantoms.record _DuplicateDefinitionNameError [
      unName _DuplicateDefinitionNameError_moduleName Phantoms.>: nsLit nsStr,
      unName _DuplicateDefinitionNameError_name Phantoms.>: nm nameStr]

duplicateDefinitionNameErr :: String -> String -> TypedTerm (Maybe InvalidModuleError)
duplicateDefinitionNameErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_duplicateDefinitionName $
    Phantoms.record _DuplicateDefinitionNameError [
      unName _DuplicateDefinitionNameError_moduleName Phantoms.>: nsLit nsStr,
      unName _DuplicateDefinitionNameError_name Phantoms.>: nm nameStr]

duplicateDefinitionNameRule :: Name
duplicateDefinitionNameRule = Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName"

duplicateModuleNameErr :: String -> TypedTerm (Maybe InvalidPackageError)
duplicateModuleNameErr nsStr = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_duplicateModuleName $
    Phantoms.record _DuplicateModuleNameError [
      unName _DuplicateModuleNameError_moduleName Phantoms.>: nsLit nsStr]

-- | An empty TypedTerm-encoded ValidationResult InvalidModuleError, used as the
-- starting accumulator for the orchestrator.
emptyVR :: TypedTerm (ValidationResult InvalidModuleError)
emptyVR = Validation.validationResult
  (Phantoms.list ([] :: [TypedTerm InvalidModuleError]))
  (Phantoms.list ([] :: [TypedTerm InvalidModuleError]))

invalidDefinitionNameErr :: String -> String -> TypedTerm CaseConvention -> TypedTerm (Maybe InvalidModuleError)
invalidDefinitionNameErr nsStr nameStr expectedConv = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_invalidDefinitionName $
    Phantoms.record _InvalidDefinitionNameError [
      unName _InvalidDefinitionNameError_moduleName Phantoms.>: nsLit nsStr,
      unName _InvalidDefinitionNameError_name Phantoms.>: nm nameStr,
      unName _InvalidDefinitionNameError_expectedConvention Phantoms.>: expectedConv]

invalidModuleNameConventionErr :: String -> TypedTerm (Maybe InvalidModuleError)
invalidModuleNameConventionErr nsStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_invalidModuleNameConvention $
    Phantoms.record _InvalidModuleNameConventionError [
      unName _InvalidModuleNameConventionError_moduleName Phantoms.>: nsLit nsStr]

invalidPackageNameErr :: String -> TypedTerm (Maybe InvalidPackageError)
invalidPackageNameErr nameStr = justPackageError $
  Phantoms.inject _InvalidPackageError _InvalidPackageError_invalidPackageName $
    Phantoms.record _InvalidPackageNameError [
      unName _InvalidPackageNameError_packageName Phantoms.>: pn nameStr]

-- ============================================================================
-- Test groups (alphabetical)
-- ============================================================================

justModuleError :: TypedTerm InvalidModuleError -> TypedTerm (Maybe InvalidModuleError)
justModuleError = Phantoms.just

justPackageError :: TypedTerm InvalidPackageError -> TypedTerm (Maybe InvalidPackageError)
justPackageError = Phantoms.just

-- | Smoke tests that the kernelModule orchestrator combines the per-check
-- validators and short-circuits on first failure.
kernelModuleTests :: TypedTermDefinition TestGroup
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
kernelPackageTests :: TypedTermDefinition TestGroup
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
      (conflictingModuleNameErr "hydra.foo" "hydra.foo")]

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

-- | Module-level convenience: run a single module-validator over an input.
mc :: String -> TypedTerm (Module -> Maybe InvalidModuleError) -> TypedTerm Module -> TypedTerm (Maybe InvalidModuleError) -> TypedTerm TestCaseWithMetadata
mc = validatePackagingModuleCase

-- | Bare InvalidModuleError for a missing-documentation finding. Differs
-- from 'missingDocumentationErr' in that it returns the bare error (not
-- Maybe), suitable for inclusion in a ValidationResult's error/warning list.
missingDocErrAt :: String -> String -> TypedTerm InvalidModuleError
missingDocErrAt nsStr nameStr =
  Phantoms.inject _InvalidModuleError _InvalidModuleError_missingDocumentation $
    Phantoms.record _MissingDocumentationError [
      unName _MissingDocumentationError_moduleName Phantoms.>: nsLit nsStr,
      unName _MissingDocumentationError_name Phantoms.>: nm nameStr]

missingDocumentationErr :: String -> String -> TypedTerm (Maybe InvalidModuleError)
missingDocumentationErr nsStr nameStr = justModuleError $
  Phantoms.inject _InvalidModuleError _InvalidModuleError_missingDocumentation $
    Phantoms.record _MissingDocumentationError [
      unName _MissingDocumentationError_moduleName Phantoms.>: nsLit nsStr,
      unName _MissingDocumentationError_name Phantoms.>: nm nameStr]

-- | Fully qualified rule names used by the profile-aware tests.
missingDocumentationRule :: Name
missingDocumentationRule = Name "hydra.error.packaging.InvalidModuleError.missingDocumentation"

-- | Build a TermDefinition for a fully-qualified name whose term carries a
-- top-level description annotation. Used for happy-path cases where every
-- definition needs to look documented to checkDefinitionDocumentation.
mkDocumentedTermDef :: String -> TypedTerm Definition
mkDocumentedTermDef fullName = Packaging.definitionTerm $ Packaging.termDefinition
  (nm fullName)
  Phantoms.nothing
  (Phantoms.nothing :: TypedTerm (Maybe TermSignature))
  documentedPlaceholderTerm

-- | Build a Module with the given namespace and definitions and no dependencies.
mkModule :: String -> [TypedTerm Definition] -> TypedTerm Module
mkModule nsStr defs = Packaging.module_
  (nsLit nsStr)
  (Phantoms.just (Packaging.entityMetadata
    (Phantoms.just $ Phantoms.string ("Test module " <> nsStr))
    (Phantoms.list ([] :: [TypedTerm String])) (Phantoms.list ([] :: [TypedTerm EntityReference])) Phantoms.nothing))
  (Phantoms.list ([] :: [TypedTerm ModuleDependency]))
  (Phantoms.list defs)

-- | Build a Package with the given name and modules.
mkPackage :: String -> [TypedTerm Module] -> TypedTerm Package
mkPackage nameStr mods = Packaging.package
  (pn nameStr)
  (Phantoms.just (Packaging.entityMetadata
    (Phantoms.just $ Phantoms.string ("Test package " <> nameStr))
    (Phantoms.list ([] :: [TypedTerm String])) (Phantoms.list ([] :: [TypedTerm EntityReference])) Phantoms.nothing))
  (Phantoms.list ([] :: [TypedTerm PackageDependency]))
  (Phantoms.list mods)

-- | Build a TermDefinition without a doc annotation (top-level term is a bare
-- literal). Used to drive checkDefinitionDocumentation failures.
mkUndocumentedTermDef :: String -> TypedTerm Definition
mkUndocumentedTermDef fullName = Packaging.definitionTerm $ Packaging.termDefinition
  (nm fullName)
  Phantoms.nothing
  (Phantoms.nothing :: TypedTerm (Maybe TermSignature))
  placeholderTerm

-- ============================================================================
-- Expected-value helpers
-- ============================================================================

-- | Build a Name from a String literal.
nm :: String -> TypedTerm Name
nm s = Core.name $ Phantoms.string s

noModuleError :: TypedTerm (Maybe InvalidModuleError)
noModuleError = Phantoms.nothing

noPackageError :: TypedTerm (Maybe InvalidPackageError)
noPackageError = Phantoms.nothing

-- | Build a ModuleName from a String literal.
nsLit :: String -> TypedTerm ModuleName
nsLit s = Packaging.moduleName2 $ Phantoms.string s

-- | Package-level convenience: run a single package-validator over an input.
pc :: String -> TypedTerm (Package -> Maybe InvalidPackageError) -> TypedTerm Package -> TypedTerm (Maybe InvalidPackageError) -> TypedTerm TestCaseWithMetadata
pc = validatePackagingPackageCase

-- | A reified Term value (a Core.TermLiteral of a Core.LiteralString) for use
-- as a placeholder body in test fixture term-definitions.
placeholderTerm :: TypedTerm Term
placeholderTerm = Core.termLiteral $ Core.literalString $ Phantoms.string "value"

-- | Build a PackageName from a String literal.
pn :: String -> TypedTerm PackageName
pn s = Packaging.packageName2 $ Phantoms.string s

profileBehaviourTests :: TypedTermDefinition TestGroup
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
          @@ profileWith [missingDocumentationRule, duplicateDefinitionNameRule] [] 5 5)
          @@ emptyVR)
          @@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa",
         duplicateDefErrAt "hydra.foo" "hydra.foo.aaa"]
        []),
    -- Warning classification: same input, both rules live in warningRules.
    -- Expect 0 errors, 2 warnings.
    universalCase "warning classification: both rules demoted to warnings"
      (showValidationResultModule
        (((validatePackagingModuleProfiledRef
          @@ profileWith [] [missingDocumentationRule, duplicateDefinitionNameRule] 5 5)
          @@ emptyVR)
          @@ twoRuleViolationModule))
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
          @@ profileWith [missingDocumentationRule] [] 5 5)
          @@ emptyVR)
          @@ twoRuleViolationModule))
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
          @@ profileWith [missingDocumentationRule, duplicateDefinitionNameRule] [] 1 5)
          @@ emptyVR)
          @@ twoRuleViolationModule))
      (showValidationResultModule $ resultWithModule
        [missingDocErrAt "hydra.foo" "hydra.foo.aaa"]
        [])]

-- | Build a ValidationProfile from explicit error/warning rule lists and bounds.
profileWith :: [Name] -> [Name] -> Int -> Int -> TypedTerm ValidationProfile
profileWith errs warns mE mW = Validation.validationProfile
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> errs)
  (Sets.fromList $ Phantoms.list $ Phantoms.nameLift <$> warns)
  (Phantoms.int32 mE)
  (Phantoms.int32 mW)

-- | Build a ValidationResult of InvalidModuleError from explicit error and warning lists.
resultWithModule
  :: [TypedTerm InvalidModuleError]
  -> [TypedTerm InvalidModuleError]
  -> TypedTerm (ValidationResult InvalidModuleError)
resultWithModule errs warns = Validation.validationResult
  (Phantoms.list errs) (Phantoms.list warns)

-- | A test fixture module that violates two distinct rules simultaneously:
-- one undocumented definition (missingDocumentation) and a duplicate
-- definition name (duplicateDefinitionName). Used by the multi-error
-- accumulation cases below.
twoRuleViolationModule :: TypedTerm Module
twoRuleViolationModule = mkModule "hydra.foo" [
  mkUndocumentedTermDef "hydra.foo.aaa",
  mkUndocumentedTermDef "hydra.foo.aaa"]
