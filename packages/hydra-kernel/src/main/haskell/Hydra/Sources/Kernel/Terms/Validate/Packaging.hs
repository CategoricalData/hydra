module Hydra.Sources.Kernel.Terms.Validate.Packaging where

-- Standard imports for kernel terms modules
import Hydra.Kernel
import           Hydra.Dsl.Bootstrap (unqualifiedDep)
import Hydra.Error.Packaging (
  InvalidModuleError, InvalidPackageError,
  _InvalidModuleError,
  _InvalidModuleError_conflictingVariantName,
  _InvalidModuleError_definitionNotInModuleName,
  _InvalidModuleError_definitionsOutOfOrder,
  _InvalidModuleError_duplicateDefinitionName,
  _InvalidModuleError_invalidDefinitionName,
  _InvalidModuleError_invalidModuleNameConvention,
  _InvalidModuleError_missingDocumentation,
  _InvalidPackageError,
  _InvalidPackageError_conflictingModuleName,
  _InvalidPackageError_duplicateModuleName,
  _InvalidPackageError_invalidPackageName)
import Hydra.Packaging (Package)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Error.Packaging       as ErrorPackaging
import qualified Hydra.Dsl.Meta.Core             as Core
import qualified Hydra.Dsl.Meta.Lib.Equality     as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists        as Lists
import qualified Hydra.Dsl.Meta.Lib.Logic        as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps         as Maps
import qualified Hydra.Dsl.Meta.Lib.Maybes       as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs        as Pairs
import qualified Hydra.Dsl.Meta.Lib.Regex        as Regex
import qualified Hydra.Dsl.Meta.Lib.Sets         as Sets
import qualified Hydra.Dsl.Meta.Lib.Strings      as Strings
import qualified Hydra.Dsl.Packaging                as Packaging
import qualified Hydra.Dsl.Packaging             as Packaging
import qualified Hydra.Dsl.Util                  as Util
import qualified Hydra.Dsl.Validation            as Validation
import           Hydra.Dsl.Meta.Phantoms         as Phantoms
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Sources.Kernel.Terms.Annotations as Annotations
import qualified Hydra.Sources.Kernel.Terms.Constants  as Constants
import qualified Hydra.Sources.Kernel.Terms.Formatting as Formatting
import qualified Hydra.Sources.Kernel.Terms.Names      as Names
import qualified Data.List                       as L
import           Prelude hiding ((++))
import qualified Data.List                       as L
import qualified Data.Set                        as S


ns :: ModuleName
ns = ModuleName "hydra.validate.packaging"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([Annotations.ns, Constants.ns, Formatting.ns, Names.ns] L.++ kernelTypesModuleNames),
            moduleDescription = Just "Validation functions for modules and packages"}
  where
    definitions = [
      toDefinition appendFindingModule,
      toDefinition appendFindingPackage,
      toDefinition checkConflictingModuleNames,
      toDefinition checkConflictingVariantNames,
      toDefinition checkDefinitionDocumentation,
      toDefinition checkDefinitionModuleNames,
      toDefinition checkDefinitionNameConvention,
      toDefinition checkDefinitionOrdering,
      toDefinition checkDuplicateDefinitionNames,
      toDefinition checkDuplicateModuleNames,
      toDefinition checkModuleNameConvention,
      toDefinition checkPackageNameConvention,
      toDefinition definitionName,
      toDefinition enabledPackaging,
      toDefinition kernelDefaultPackagingProfile,
      toDefinition kernelModule,
      toDefinition kernelPackage,
      toDefinition module',
      toDefinition package]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

-- ============================================================================
-- Profile helpers (host-side)
-- ============================================================================

-- | Compose a fully qualified rule identifier from a union-type qualified
-- name and a variant local name, joined with '.'. Mirrors the
-- 'qualifiedRule' helper in Validate/Core.hs. Used at profile-construction
-- time to derive rule IDs like
-- 'hydra.error.packaging.InvalidModuleError.duplicateDefinitionName' from
-- the generated _InvalidModuleError and _InvalidModuleError_duplicateDefinitionName
-- constants.
qualifiedRule :: Name -> Name -> Name
qualifiedRule (Name u) (Name v) = Name (L.concat [u, ".", v])

-- | Wrap a leaf-shaped 'Maybe InvalidModuleError' finding with the rule that
-- produced it, gated by the active profile. If the rule's qualified name is
-- not in either errorRules or warningRules, the inner finding expression is
-- never evaluated. When the rule is enabled and the inner finding is Just,
-- returns 'Just (ruleName, payload)'; otherwise 'Nothing'.
guardedModuleRule
  :: TTerm ValidationProfile
  -> Name -- ^ Union-type qualified name (e.g. _InvalidModuleError).
  -> Name -- ^ Variant local name (e.g. _InvalidModuleError_duplicateDefinitionName).
  -> TTerm (Maybe InvalidModuleError) -- ^ The leaf-shaped finding term.
  -> TTerm (Maybe (Name, InvalidModuleError))
guardedModuleRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledPackaging @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | Package-side counterpart of 'guardedModuleRule'.
guardedPackageRule
  :: TTerm ValidationProfile
  -> Name
  -> Name
  -> TTerm (Maybe InvalidPackageError)
  -> TTerm (Maybe (Name, InvalidPackageError))
guardedPackageRule profile unionName variantName findingExpr =
  Logic.ifElse (enabledPackaging @@ profile @@ ruleNameTerm)
    (Maybes.map ("f" ~> pair ruleNameTerm (var "f")) findingExpr)
    nothing
  where
    ruleNameTerm = nameLift (qualifiedRule unionName variantName)

-- | An empty ValidationResult (no errors, no warnings) parameterized by 'e'.
-- Used as the initial accumulator. Defined locally to avoid a cross-module
-- term dependency on hydra.validate.core.
emptyResult :: TTerm (ValidationResult e)
emptyResult = Validation.validationResult
  (list ([] :: [TTerm e]))
  (list ([] :: [TTerm e]))

-- ============================================================================
-- Profile-aware helpers (DSL definitions)
-- ============================================================================

-- | Classify a rule-tagged 'Maybe (Name, InvalidModuleError)' finding against
-- the active profile and append the payload (without its rule tag) to the
-- appropriate list in the accumulator. Findings whose rule appears in
-- 'errorRules' go to the errors list; findings whose rule appears in
-- 'warningRules' go to the warnings list. Each list respects its bound
-- ('maxErrors' / 'maxWarnings'); attempting to append past a bound is a
-- silent drop. A 'Nothing' input leaves the accumulator unchanged.
appendFindingModule :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidModuleError
  -> Maybe (Name, InvalidModuleError)
  -> ValidationResult InvalidModuleError)
appendFindingModule = define "appendFindingModule" $
  doc "Append a rule-tagged InvalidModuleError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Package-side counterpart of 'appendFindingModule'.
appendFindingPackage :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidPackageError
  -> Maybe (Name, InvalidPackageError)
  -> ValidationResult InvalidPackageError)
appendFindingPackage = define "appendFindingPackage" $
  doc "Append a rule-tagged InvalidPackageError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds." $
  "p" ~> "acc" ~> "finding" ~>
  Maybes.cases (var "finding")
    (var "acc")
    ("rp" ~>
      "ruleName" <~ Pairs.first (var "rp") $
      "payload" <~ Pairs.second (var "rp") $
      "errs" <~ Validation.validationResultErrors (var "acc") $
      "wrns" <~ Validation.validationResultWarnings (var "acc") $
      Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
        (Logic.ifElse (Equality.lt (Lists.length $ var "errs") (Validation.validationProfileMaxErrors $ var "p"))
          (Validation.validationResult
            (Lists.concat2 (var "errs") (Lists.singleton $ var "payload"))
            (var "wrns"))
          (var "acc"))
        (Logic.ifElse (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))
          (Logic.ifElse (Equality.lt (Lists.length $ var "wrns") (Validation.validationProfileMaxWarnings $ var "p"))
            (Validation.validationResult
              (var "errs")
              (Lists.concat2 (var "wrns") (Lists.singleton $ var "payload")))
            (var "acc"))
          (var "acc")))

-- | Extract the name from a Definition (term or type)
definitionName :: TTermDefinition (Definition -> Name)
definitionName = define "definitionName" $
  doc "Extract the name from a definition" $
  "def" ~>
  cases _Definition (var "def") Nothing [
    _Definition_term>>: "td" ~> Packaging.termDefinitionName (var "td"),
    _Definition_type>>: "td" ~> Packaging.typeDefinitionName (var "td")]

-- | Check for module namespaces that would conflict when mapped to a target language's
-- directory structure. Two namespaces conflict if they are identical when lowercased,
-- e.g. hydra.fooBar and hydra.foobar, or hydra.Foo.Bar and hydra.foo.bar.
-- Fails on the first conflict found.
checkConflictingModuleNames :: TTermDefinition (Package -> Maybe InvalidPackageError)
checkConflictingModuleNames = define "checkConflictingModuleNames" $
  doc "Check for module namespaces that conflict when mapped to target language paths" $
  "pkg" ~>
  -- Build a map from lowercased namespace strings to original namespaces.
  -- If a lowercased version is already in the map, that's a conflict.
  "result" <~ Lists.foldl
    ("acc" ~> "mod" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("ns" <~ Packaging.moduleName (var "mod") $
          "key" <~ Strings.toLower (Packaging.unModuleName $ var "ns") $
          "existing" <~ Maps.lookup (var "key") (var "seen") $
          Maybes.cases (var "existing")
            -- No conflict: add to map
            (pair (Maps.insert (var "key") (var "ns") (var "seen")) nothing)
            -- Conflict found
            ("first" ~>
              pair (var "seen") (just $
                ErrorPackaging.invalidPackageErrorConflictingModuleName $
                  ErrorPackaging.conflictingModuleNameError (var "first") (var "ns"))))
        (constant $ var "acc"))
    (pair Maps.empty nothing)
    (Packaging.packageModules $ var "pkg") $
  Pairs.second (var "result")

-- | Check for union variant names that conflict with type definition names.
-- For each union type, the capitalized type local name concatenated with
-- the capitalized variant field name must not collide with any other type
-- definition's local name in the same module.
checkConflictingVariantNames :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkConflictingVariantNames = define "checkConflictingVariantNames" $
  doc "Check for union variant names that, when mapped to constructor names, conflict with other type definitions" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  "defs" <~ Packaging.moduleDefinitions (var "mod") $
  -- Collect all definition local names into a set
  "defNames" <~ Lists.foldl
    ("acc" ~> "def" ~>
      Sets.insert (Names.localNameOf @@ (definitionName @@ var "def")) (var "acc"))
    Sets.empty
    (var "defs") $
  -- For each type definition that is a union, check each field
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        (cases _Definition (var "def") (Just nothing) [
          _Definition_type>>: "td" ~>
            "typeName" <~ Packaging.typeDefinitionName (var "td") $
            "localTypeName" <~ (Names.localNameOf @@ var "typeName") $
            "typ" <~ (Core.typeSchemeBody $ Packaging.typeDefinitionTypeScheme (var "td")) $
            cases _Type (var "typ") (Just nothing) [
              _Type_union>>: "fields" ~>
                -- Check each field of the union
                Lists.foldl
                  ("innerAcc" ~> "field" ~>
                    Maybes.cases (var "innerAcc")
                      ("fieldName" <~ Core.fieldTypeName (var "field") $
                        "localFieldName" <~ (Names.localNameOf @@ var "fieldName") $
                        "constructorName" <~ Strings.cat2
                          (Formatting.capitalize @@ var "localTypeName")
                          (Formatting.capitalize @@ var "localFieldName") $
                        Logic.ifElse (Sets.member (var "constructorName") (var "defNames"))
                          (just $ ErrorPackaging.invalidModuleErrorConflictingVariantName $
                            ErrorPackaging.conflictingVariantNameError
                              (var "ns")
                              (var "typeName")
                              (var "fieldName")
                              (Core.name $ var "constructorName"))
                          nothing)
                      (constant $ var "innerAcc"))
                  nothing
                  (var "fields")]])
        (constant $ var "acc"))
    nothing
    (var "defs")

-- | Check that every term-level definition's term, and every type-level definition's
-- type-scheme body, carries a description annotation at its outermost layer.
-- A definition whose top-level Term constructor is anything other than _Term_annotated,
-- or whose top-level annotation map lacks the description key, is reported as missing
-- documentation. This validator does not descend into the body of a term or type.
-- Fails on the first undocumented definition found.
checkDefinitionDocumentation :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDefinitionDocumentation = define "checkDefinitionDocumentation" $
  doc "Check that every top-level definition is wrapped in a description annotation" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        ("name" <~ (definitionName @@ var "def") $
          "documented" <~ cases _Definition (var "def") (Just false) [
            _Definition_term>>: "td" ~>
              "term" <~ Packaging.termDefinitionTerm (var "td") $
              cases _Term (var "term") (Just false) [
                _Term_annotated>>: "at" ~>
                  Annotations.hasDescription @@ (Core.annotatedTermAnnotation $ var "at")],
            _Definition_type>>: "td" ~>
              "typ" <~ (Core.typeSchemeBody $ Packaging.typeDefinitionTypeScheme (var "td")) $
              cases _Type (var "typ") (Just false) [
                _Type_annotated>>: "at" ~>
                  Annotations.hasDescription @@ (Core.annotatedTypeAnnotation $ var "at")]] $
          Logic.ifElse (var "documented")
            nothing
            (just $ ErrorPackaging.invalidModuleErrorMissingDocumentation $
              ErrorPackaging.missingDocumentationError (var "ns") (var "name")))
        (constant $ var "acc"))
    nothing
    (Packaging.moduleDefinitions $ var "mod")

-- | Check that all definition names in a module have the module's name as a prefix.
-- For a module named foo.bar, every definition name must have the form foo.bar.xyz.
-- Fails on the first definition that violates this constraint.
checkDefinitionModuleNames :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDefinitionModuleNames = define "checkDefinitionModuleNames" $
  doc "Check that all definition names in a module have the module's name as a prefix" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  "prefix" <~ (Strings.cat2 (Packaging.unModuleName $ var "ns") (string ".")) $
  "prefixLen" <~ Strings.length (var "prefix") $
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        -- No error yet: check this definition
        ("name" <~ (definitionName @@ var "def") $
          "nameStr" <~ Core.unName (var "name") $
          "namePrefix" <~ Lists.take (var "prefixLen") (Strings.toList $ var "nameStr") $
          Logic.ifElse (Equality.equal (Strings.fromList $ var "namePrefix") (var "prefix"))
            nothing
            (just $ ErrorPackaging.invalidModuleErrorDefinitionNotInModuleName $
              ErrorPackaging.definitionNotInModuleNameError (var "ns") (var "name")))
        -- Already have an error: stop
        (constant $ var "acc"))
    nothing
    (Packaging.moduleDefinitions $ var "mod")

-- | Check that every term-level definition's local name matches the camelCase regex
-- and every type-level definition's local name matches the PascalCase regex.
-- Fails on the first definition whose name violates the convention.
checkDefinitionNameConvention :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDefinitionNameConvention = define "checkDefinitionNameConvention" $
  doc "Check that term definitions have camelCase local names and type definitions have PascalCase local names" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  Lists.foldl
    ("acc" ~> "def" ~>
      Maybes.cases (var "acc")
        ("name" <~ (definitionName @@ var "def") $
          "local" <~ (Names.localNameOf @@ var "name") $
          "expected" <~ cases _Definition (var "def") (Just Util.caseConventionCamel) [
            _Definition_term>>: constant Util.caseConventionCamel,
            _Definition_type>>: constant Util.caseConventionPascal] $
          "pattern" <~ cases _Definition (var "def") (Just (Phantoms.asTerm Constants.regexCamelCase)) [
            _Definition_term>>: constant (Phantoms.asTerm Constants.regexCamelCase),
            _Definition_type>>: constant (Phantoms.asTerm Constants.regexPascalCase)] $
          Logic.ifElse (Regex.matches (var "pattern") (var "local"))
            nothing
            (just $ ErrorPackaging.invalidModuleErrorInvalidDefinitionName $
              ErrorPackaging.invalidDefinitionNameError (var "ns") (var "name") (var "expected")))
        (constant $ var "acc"))
    nothing
    (Packaging.moduleDefinitions $ var "mod")

-- | Check that the module's definitions list is sorted in ascending lexicographic
-- order by local name. The check is a pairwise walk over consecutive entries; if any
-- entry's local name is less than or equal to its predecessor's, an error is reported
-- naming both the preceding and following definitions. The check inspects the order
-- of the definitions list itself, not the order of declarations in the source file.
-- Fails on the first out-of-order pair found.
--
-- This check is appropriate only for hand-written Source modules. Generator-derived
-- modules (e.g. 'hydra.dsl.*', 'hydra.encode.*', 'hydra.decode.*') deliberately use
-- a semantic grouping by source type and would always fail this check; do not run
-- it against them.
checkDefinitionOrdering :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDefinitionOrdering = define "checkDefinitionOrdering" $
  doc "Check that a module's definitions list is sorted in ascending lexicographic order by local name" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  -- Fold through definitions tracking the previous name and any error.
  -- Accumulator is (Maybe Name, Maybe InvalidModuleError).
  "result" <~ Lists.foldl
    ("acc" ~> "def" ~>
      "prev" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("currName" <~ (definitionName @@ var "def") $
          "currLocal" <~ (Names.localNameOf @@ var "currName") $
          Maybes.cases (var "prev")
            -- First entry: no comparison needed
            (pair (just $ var "currName") nothing)
            -- Subsequent entries: compare local names
            ("prevName" ~>
              "prevLocal" <~ (Names.localNameOf @@ var "prevName") $
              Logic.ifElse (Equality.lt (var "prevLocal") (var "currLocal"))
                (pair (just $ var "currName") nothing)
                (pair (just $ var "currName") (just $
                  ErrorPackaging.invalidModuleErrorDefinitionsOutOfOrder $
                    ErrorPackaging.definitionsOutOfOrderError
                      (var "ns")
                      (var "prevName")
                      (var "currName")))))
        (constant $ var "acc"))
    (pair nothing nothing)
    (Packaging.moduleDefinitions $ var "mod") $
  Pairs.second (var "result")

-- | Check for duplicate definition names in a module.
-- Fails on the first duplicate found.
checkDuplicateDefinitionNames :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkDuplicateDefinitionNames = define "checkDuplicateDefinitionNames" $
  doc "Check for duplicate definition names in a module" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  -- Fold through definitions tracking seen names in a set.
  -- Accumulator is (Set Name, Maybe InvalidModuleError).
  "result" <~ Lists.foldl
    ("acc" ~> "def" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        -- No error yet: check this definition
        ("name" <~ (definitionName @@ var "def") $
          Logic.ifElse (Sets.member (var "name") (var "seen"))
            (pair (var "seen") (just $
              ErrorPackaging.invalidModuleErrorDuplicateDefinitionName $
                ErrorPackaging.duplicateDefinitionNameError (var "ns") (var "name")))
            (pair (Sets.insert (var "name") (var "seen")) nothing))
        -- Already have an error: stop
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (Packaging.moduleDefinitions $ var "mod") $
  Pairs.second (var "result")

-- | Check for duplicate module namespaces in a package.
-- Fails on the first duplicate found.
checkDuplicateModuleNames :: TTermDefinition (Package -> Maybe InvalidPackageError)
checkDuplicateModuleNames = define "checkDuplicateModuleNames" $
  doc "Check for duplicate module namespaces in a package" $
  "pkg" ~>
  "result" <~ Lists.foldl
    ("acc" ~> "mod" ~>
      "seen" <~ Pairs.first (var "acc") $
      "err" <~ Pairs.second (var "acc") $
      Maybes.cases (var "err")
        ("ns" <~ Packaging.moduleName (var "mod") $
          Logic.ifElse (Sets.member (var "ns") (var "seen"))
            (pair (var "seen") (just $
              ErrorPackaging.invalidPackageErrorDuplicateModuleName $
                ErrorPackaging.duplicateModuleNameError (var "ns")))
            (pair (Sets.insert (var "ns") (var "seen")) nothing))
        (constant $ var "acc"))
    (pair Sets.empty nothing)
    (Packaging.packageModules $ var "pkg") $
  Pairs.second (var "result")

-- | Check that the module's namespace matches the dotted-lowercase namespace regex
-- (dot-separated lowercase segments, each starting with a letter).
checkModuleNameConvention :: TTermDefinition (Module -> Maybe InvalidModuleError)
checkModuleNameConvention = define "checkModuleNameConvention" $
  doc "Check that the module's namespace matches the dotted-lowercase naming convention" $
  "mod" ~>
  "ns" <~ Packaging.moduleName (var "mod") $
  Logic.ifElse (Regex.matches (Phantoms.asTerm Constants.regexNamespace) (Packaging.unModuleName $ var "ns"))
    nothing
    (just $ ErrorPackaging.invalidModuleErrorInvalidModuleNameConvention $
      ErrorPackaging.invalidModuleNameConventionError (var "ns"))

-- | Check that the package's name matches the hyphen-separated lowercase package-name regex
-- (hyphen-separated lowercase segments, each starting with a letter).
checkPackageNameConvention :: TTermDefinition (Package -> Maybe InvalidPackageError)
checkPackageNameConvention = define "checkPackageNameConvention" $
  doc "Check that the package's name matches the hyphen-separated lowercase naming convention" $
  "pkg" ~>
  "pname" <~ Packaging.packageName (var "pkg") $
  Logic.ifElse (Regex.matches (Phantoms.asTerm Constants.regexPackageName) (Packaging.unPackageName $ var "pname"))
    nothing
    (just $ ErrorPackaging.invalidPackageErrorInvalidPackageName $
      ErrorPackaging.invalidPackageNameError (var "pname"))

-- ============================================================================
-- ValidationProfile-aware orchestrators
-- ============================================================================

-- | Test whether a rule is active in a profile (in either errorRules or
-- warningRules). Rules that are inactive are skipped entirely. Local
-- counterpart of 'enabled' in Validate/Core.hs; named 'enabledPackaging'
-- to avoid name clash if both modules are imported into the same scope
-- through 'Hydra.Validate.Packaging' / 'Hydra.Validate.Core'.
enabledPackaging :: TTermDefinition (ValidationProfile -> Name -> Bool)
enabledPackaging = define "enabledPackaging" $
  doc "True iff the given rule name appears in the profile's errorRules or warningRules." $
  "p" ~> "ruleName" ~>
  Logic.or
    (Sets.member (var "ruleName") (Validation.validationProfileErrorRules $ var "p"))
    (Sets.member (var "ruleName") (Validation.validationProfileWarningRules $ var "p"))

-- | The default validation profile for hydra.validate.packaging — i.e. the
-- kernel-strict packaging profile. Every per-module and per-package check
-- shipped by this module is classified as an error; no warnings;
-- 'maxErrors = 1' preserves the legacy 'first error wins' behaviour;
-- 'maxWarnings = 20' is a deliberately small starting cap.
--
-- Callers who want a less strict profile (e.g. non-kernel modules where
-- naming-convention or alphabetical-ordering violations should not block)
-- should construct their own profile by removing rules from this one.
kernelDefaultPackagingProfile :: TTermDefinition ValidationProfile
kernelDefaultPackagingProfile = define "kernelDefaultPackagingProfile" $
  doc "The default validation profile for module/package validation. Every kernel-shipped check classified as an error; no warnings; maxErrors=1, maxWarnings=20." $
  Validation.validationProfile
    (Sets.fromList $ list $ nameLift <$> kernelPackagingRuleNames)
    Sets.empty
    (int32 1)
    (int32 20)

-- | The full set of rule names classified as errors in
-- 'kernelDefaultPackagingProfile'. Single source of truth: any rule
-- wired into 'module'' or 'package' must appear here.
--
-- Note: the 'InvalidPackageError.invalidModule' variant is deliberately
-- excluded. It is not a per-rule check; it is the wrapper used by
-- 'package' to lift module-level findings into package-level findings.
-- That lift is unconditional: module errors are always promoted, governed
-- only by the same profile via the per-module rules. Including
-- 'invalidModule' in a profile would have no effect on validator
-- behaviour and would be misleading to callers.
kernelPackagingRuleNames :: [Name]
kernelPackagingRuleNames = L.concat
  [ fmap (qualifiedRule _InvalidModuleError)
      [ _InvalidModuleError_conflictingVariantName
      , _InvalidModuleError_definitionNotInModuleName
      , _InvalidModuleError_definitionsOutOfOrder
      , _InvalidModuleError_duplicateDefinitionName
      , _InvalidModuleError_invalidDefinitionName
      , _InvalidModuleError_invalidModuleNameConvention
      , _InvalidModuleError_missingDocumentation]
  , fmap (qualifiedRule _InvalidPackageError)
      [ _InvalidPackageError_conflictingModuleName
      , _InvalidPackageError_duplicateModuleName
      , _InvalidPackageError_invalidPackageName]]

-- | Validate a module against the given ValidationProfile, threading a
-- 'ValidationResult InvalidModuleError' accumulator. Each per-rule check
-- is wrapped with 'guardedModuleRule', so checks whose rule is absent
-- from both 'errorRules' and 'warningRules' are skipped entirely.
-- Findings are classified into errors vs warnings per the profile and
-- bounded by 'maxErrors' / 'maxWarnings'. Errors hard-stop the rule
-- sequence once the cap is reached; warnings continue accumulating up to
-- 'maxWarnings' but never cause termination.
module' :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidModuleError
  -> Module
  -> ValidationResult InvalidModuleError)
module' = define "module" $
  doc "Validate a module against the given ValidationProfile, accumulating findings into a ValidationResult. Errors hard-stop the rule sequence once maxErrors is reached." $
  "p" ~> "acc0" ~> "mod" ~>
  -- Each rule check is run in turn; appendFindingModule classifies the
  -- finding (if any) and re-checks the cap. The cap is also re-checked
  -- explicitly between rules via 'foldl' over a list of guarded checks
  -- so we don't waste work running a rule whose finding would be dropped.
  Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingModule @@ var "p" @@ var "acc" @@ var "guarded"))
    (var "acc0")
    (list [
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_conflictingVariantName
        (checkConflictingVariantNames @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_missingDocumentation
        (checkDefinitionDocumentation @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_invalidDefinitionName
        (checkDefinitionNameConvention @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_definitionNotInModuleName
        (checkDefinitionModuleNames @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_definitionsOutOfOrder
        (checkDefinitionOrdering @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_duplicateDefinitionName
        (checkDuplicateDefinitionNames @@ var "mod"),
      guardedModuleRule (var "p") _InvalidModuleError _InvalidModuleError_invalidModuleNameConvention
        (checkModuleNameConvention @@ var "mod")])

-- | Validate a package against the given ValidationProfile, threading a
-- 'ValidationResult InvalidPackageError' accumulator. Runs the
-- package-level rules first, then walks the package's modules, lifting
-- each module-level error/warning into an InvalidPackageError via
-- 'invalidModule'. Hard-stops once the errors list reaches 'maxErrors'.
package :: TTermDefinition (
  ValidationProfile
  -> ValidationResult InvalidPackageError
  -> Package
  -> ValidationResult InvalidPackageError)
package = define "package" $
  doc "Validate a package against the given ValidationProfile, accumulating findings into a ValidationResult. Errors hard-stop traversal once maxErrors is reached." $
  "p" ~> "acc0" ~> "pkg" ~>
  -- First: run the package-level rules sequentially through appendFindingPackage.
  "accPkg" <~ Lists.foldl
    ("acc" ~> "guarded" ~>
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        (appendFindingPackage @@ var "p" @@ var "acc" @@ var "guarded"))
    (var "acc0")
    (list [
      guardedPackageRule (var "p") _InvalidPackageError _InvalidPackageError_conflictingModuleName
        (checkConflictingModuleNames @@ var "pkg"),
      guardedPackageRule (var "p") _InvalidPackageError _InvalidPackageError_duplicateModuleName
        (checkDuplicateModuleNames @@ var "pkg"),
      guardedPackageRule (var "p") _InvalidPackageError _InvalidPackageError_invalidPackageName
        (checkPackageNameConvention @@ var "pkg")]) $
  -- Second: walk each module, lifting module-level findings into
  -- package-level findings via invalidModule. Each step produces a fresh
  -- ValidationResult InvalidModuleError starting from emptyResult,
  -- whose contents we then re-append to the running package-level
  -- accumulator. 'invalidModule' is the InvalidPackageError wrapper.
  Lists.foldl
    ("acc" ~> "mod" ~>
      Logic.ifElse
        (Equality.gte
          (Lists.length $ Validation.validationResultErrors $ var "acc")
          (Validation.validationProfileMaxErrors $ var "p"))
        (var "acc")
        ("mr" <~ (module' @@ var "p" @@ emptyResult @@ var "mod") $
          -- Lift module-level errors (and warnings) into package-level
          -- findings. The resulting list is appended in order to 'acc'
          -- subject to the profile bounds.
          "liftedErrs" <~ Lists.map
            ("e" ~> ErrorPackaging.invalidPackageErrorInvalidModule (var "e"))
            (Validation.validationResultErrors $ var "mr") $
          "liftedWrns" <~ Lists.map
            ("w" ~> ErrorPackaging.invalidPackageErrorInvalidModule (var "w"))
            (Validation.validationResultWarnings $ var "mr") $
          -- Concat lists and re-bound to maxErrors / maxWarnings via Lists.take.
          "newErrs" <~ Lists.take
            (Validation.validationProfileMaxErrors $ var "p")
            (Lists.concat2 (Validation.validationResultErrors $ var "acc") (var "liftedErrs")) $
          "newWrns" <~ Lists.take
            (Validation.validationProfileMaxWarnings $ var "p")
            (Lists.concat2 (Validation.validationResultWarnings $ var "acc") (var "liftedWrns")) $
          Validation.validationResult (var "newErrs") (var "newWrns")))
    (var "accPkg")
    (Packaging.packageModules $ var "pkg")

-- ============================================================================
-- Convenience entry points (kernel-strict)
-- ============================================================================

-- | Validate a module against the kernel-default packaging profile,
-- returning the first error found or 'Nothing' if valid. Equivalent to
-- 'module'' applied to 'kernelDefaultPackagingProfile' with an empty
-- accumulator, then head-extracted; provided as a named entry point for
-- the common case of validating a kernel module under the strict
-- kernel-shipped rule set.
--
-- Intended for hand-written Source modules only (the ones that contribute
-- to 'Sources.kernelModules'). Do not apply to generator-derived modules
-- such as the 'hydra.dsl.*', 'hydra.encode.*', or 'hydra.decode.*'
-- families: their 'definitions' lists follow a semantic grouping which
-- would fail 'checkDefinitionOrdering'. Callers wanting custom rule
-- sets, multi-error accumulation, or warnings should use 'module'' with
-- an explicit 'ValidationProfile'.
kernelModule :: TTermDefinition (Module -> Maybe InvalidModuleError)
kernelModule = define "kernelModule" $
  doc "Validate a kernel module against all kernel-default packaging rules; returns the first error found or nothing if valid. Convenience wrapper around 'module'' with 'kernelDefaultPackagingProfile'." $
  "mod" ~>
  Lists.maybeHead $ Validation.validationResultErrors $
    module' @@ kernelDefaultPackagingProfile @@ emptyResult @@ var "mod"

-- | Validate a package against the kernel-default packaging profile.
-- See 'kernelModule' for the rationale and applicability constraints.
kernelPackage :: TTermDefinition (Package -> Maybe InvalidPackageError)
kernelPackage = define "kernelPackage" $
  doc "Validate a kernel package against all kernel-default packaging rules; returns the first error found or nothing if valid. Convenience wrapper around 'package' with 'kernelDefaultPackagingProfile'." $
  "pkg" ~>
  Lists.maybeHead $ Validation.validationResultErrors $
    package @@ kernelDefaultPackagingProfile @@ emptyResult @@ var "pkg"
