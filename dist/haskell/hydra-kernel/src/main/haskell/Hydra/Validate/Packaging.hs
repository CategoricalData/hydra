-- Note: this is an automatically generated file. Do not edit.
-- | Validation functions for modules and packages

module Hydra.Validate.Packaging where
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Regex as Regex
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Append a rule-tagged InvalidModuleError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFindingModule :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingModule p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Append a rule-tagged InvalidPackageError finding to a ValidationResult, classifying as error or warning per the profile and respecting maxErrors/maxWarnings bounds.
appendFindingPackage :: Validation.ValidationProfile -> Validation.ValidationResult t0 -> Maybe (Core.Name, t0) -> Validation.ValidationResult t0
appendFindingPackage p acc finding =
    Maybes.cases finding acc (\rp ->
      let ruleName = Pairs.first rp
          payload = Pairs.second rp
          errs = Validation.validationResultErrors acc
          wrns = Validation.validationResultWarnings acc
      in (Logic.ifElse (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Logic.ifElse (Equality.lt (Lists.length errs) (Validation.validationProfileMaxErrors p)) (Validation.ValidationResult {
        Validation.validationResultErrors = (Lists.concat2 errs (Lists.singleton payload)),
        Validation.validationResultWarnings = wrns}) acc) (Logic.ifElse (Sets.member ruleName (Validation.validationProfileWarningRules p)) (Logic.ifElse (Equality.lt (Lists.length wrns) (Validation.validationProfileMaxWarnings p)) (Validation.ValidationResult {
        Validation.validationResultErrors = errs,
        Validation.validationResultWarnings = (Lists.concat2 wrns (Lists.singleton payload))}) acc) acc)))
-- | Check for module namespaces that conflict when mapped to target language paths
checkConflictingModuleNames :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
checkConflictingModuleNames pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging.moduleName mod
                      key = Strings.toLower (Packaging.unModuleName ns)
                      existing = Maps.lookup key seen
                  in (Maybes.cases existing (Maps.insert key ns seen, Nothing) (\first -> (
                    seen,
                    (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleName (ErrorPackaging.ConflictingModuleNameError {
                      ErrorPackaging.conflictingModuleNameErrorFirst = first,
                      ErrorPackaging.conflictingModuleNameErrorSecond = ns}))))))) (\_ -> acc))) (Maps.empty, Nothing) (Packaging.packageModules pkg)
      in (Pairs.second result)
-- | Check for union variant names that, when mapped to constructor names, conflict with other type definitions
checkConflictingVariantNames :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkConflictingVariantNames mod =

      let ns = Packaging.moduleName mod
          defs = Packaging.moduleDefinitions mod
          defNames = Lists.foldl (\acc -> \def -> Sets.insert (Names.localNameOf (definitionName def)) acc) Sets.empty defs
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (case def of
        Packaging.DefinitionType v0 ->
          let typeName = Packaging.typeDefinitionName v0
              localTypeName = Names.localNameOf typeName
              typ = Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)
          in case typ of
            Core.TypeUnion v1 -> Lists.foldl (\innerAcc -> \field -> Maybes.cases innerAcc (
              let fieldName = Core.fieldTypeName field
                  localFieldName = Names.localNameOf fieldName
                  constructorName = Strings.cat2 (Formatting.capitalize localTypeName) (Formatting.capitalize localFieldName)
              in (Logic.ifElse (Sets.member constructorName defNames) (Just (ErrorPackaging.InvalidModuleErrorConflictingVariantName (ErrorPackaging.ConflictingVariantNameError {
                ErrorPackaging.conflictingVariantNameErrorModuleName = ns,
                ErrorPackaging.conflictingVariantNameErrorTypeName = typeName,
                ErrorPackaging.conflictingVariantNameErrorVariantName = fieldName,
                ErrorPackaging.conflictingVariantNameErrorConflictingName = (Core.Name constructorName)}))) Nothing)) (\_ -> innerAcc)) Nothing v1
            _ -> Nothing
        _ -> Nothing) (\_ -> acc)) Nothing defs)
-- | Check that every top-level definition is wrapped in a description annotation
checkDefinitionDocumentation :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionDocumentation mod =

      let ns = Packaging.moduleName mod
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (
        let name = definitionName def
            documented =
                    case def of
                      Packaging.DefinitionTerm v0 ->
                        let term = Packaging.termDefinitionTerm v0
                        in case term of
                          Core.TermAnnotated v1 -> Annotations.hasDescription (Core.annotatedTermAnnotation v1)
                          _ -> False
                      Packaging.DefinitionType v0 ->
                        let typ = Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)
                        in case typ of
                          Core.TypeAnnotated v1 -> Annotations.hasDescription (Core.annotatedTypeAnnotation v1)
                          _ -> False
                      _ -> False
        in (Logic.ifElse documented Nothing (Just (ErrorPackaging.InvalidModuleErrorMissingDocumentation (ErrorPackaging.MissingDocumentationError {
          ErrorPackaging.missingDocumentationErrorModuleName = ns,
          ErrorPackaging.missingDocumentationErrorName = name}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that all definition names in a module have the module's name as a prefix
checkDefinitionModuleNames :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionModuleNames mod =

      let ns = Packaging.moduleName mod
          prefix = Strings.cat2 (Packaging.unModuleName ns) "."
          prefixLen = Strings.length prefix
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (
        let name = definitionName def
            nameStr = Core.unName name
            namePrefix = Lists.take prefixLen (Strings.toList nameStr)
        in (Logic.ifElse (Equality.equal (Strings.fromList namePrefix) prefix) Nothing (Just (ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleName (ErrorPackaging.DefinitionNotInModuleNameError {
          ErrorPackaging.definitionNotInModuleNameErrorModuleName = ns,
          ErrorPackaging.definitionNotInModuleNameErrorName = name}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that term definitions have camelCase local names and type definitions have PascalCase local names
checkDefinitionNameConvention :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionNameConvention mod =

      let ns = Packaging.moduleName mod
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (
        let name = definitionName def
            local = Names.localNameOf name
            expected =
                    case def of
                      Packaging.DefinitionTerm _ -> Util.CaseConventionCamel
                      Packaging.DefinitionType _ -> Util.CaseConventionPascal
                      _ -> Util.CaseConventionCamel
            pattern =
                    case def of
                      Packaging.DefinitionTerm _ -> Constants.regexCamelCase
                      Packaging.DefinitionType _ -> Constants.regexPascalCase
                      _ -> Constants.regexCamelCase
        in (Logic.ifElse (Regex.matches pattern local) Nothing (Just (ErrorPackaging.InvalidModuleErrorInvalidDefinitionName (ErrorPackaging.InvalidDefinitionNameError {
          ErrorPackaging.invalidDefinitionNameErrorModuleName = ns,
          ErrorPackaging.invalidDefinitionNameErrorName = name,
          ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = expected}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that a module's definitions list is sorted in ascending lexicographic order by local name
checkDefinitionOrdering :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionOrdering mod =

      let ns = Packaging.moduleName mod
          result =
                  Lists.foldl (\acc -> \def ->
                    let prev = Pairs.first acc
                        err = Pairs.second acc
                    in (Maybes.cases err (
                      let currName = definitionName def
                          currLocal = Names.localNameOf currName
                      in (Maybes.cases prev (Just currName, Nothing) (\prevName ->
                        let prevLocal = Names.localNameOf prevName
                        in (Logic.ifElse (Equality.lt prevLocal currLocal) (Just currName, Nothing) (
                          Just currName,
                          (Just (ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder (ErrorPackaging.DefinitionsOutOfOrderError {
                            ErrorPackaging.definitionsOutOfOrderErrorModuleName = ns,
                            ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = prevName,
                            ErrorPackaging.definitionsOutOfOrderErrorFollowingName = currName})))))))) (\_ -> acc))) (Nothing, Nothing) (Packaging.moduleDefinitions mod)
      in (Pairs.second result)
-- | Check for duplicate definition names in a module
checkDuplicateDefinitionNames :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDuplicateDefinitionNames mod =

      let ns = Packaging.moduleName mod
          result =
                  Lists.foldl (\acc -> \def ->
                    let seen = Pairs.first acc
                        err = Pairs.second acc
                    in (Maybes.cases err (
                      let name = definitionName def
                      in (Logic.ifElse (Sets.member name seen) (
                        seen,
                        (Just (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                          ErrorPackaging.duplicateDefinitionNameErrorModuleName = ns,
                          ErrorPackaging.duplicateDefinitionNameErrorName = name})))) (Sets.insert name seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging.moduleDefinitions mod)
      in (Pairs.second result)
-- | Check for duplicate module namespaces in a package
checkDuplicateModuleNames :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
checkDuplicateModuleNames pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging.moduleName mod
                  in (Logic.ifElse (Sets.member ns seen) (
                    seen,
                    (Just (ErrorPackaging.InvalidPackageErrorDuplicateModuleName (ErrorPackaging.DuplicateModuleNameError {
                      ErrorPackaging.duplicateModuleNameErrorModuleName = ns})))) (Sets.insert ns seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging.packageModules pkg)
      in (Pairs.second result)
-- | Check that the module's namespace matches the dotted-lowercase naming convention
checkModuleNameConvention :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkModuleNameConvention mod =

      let ns = Packaging.moduleName mod
      in (Logic.ifElse (Regex.matches Constants.regexNamespace (Packaging.unModuleName ns)) Nothing (Just (ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention (ErrorPackaging.InvalidModuleNameConventionError {
        ErrorPackaging.invalidModuleNameConventionErrorModuleName = ns}))))
-- | Check that the package's name matches the hyphen-separated lowercase naming convention
checkPackageNameConvention :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
checkPackageNameConvention pkg =

      let pname = Packaging.packageName pkg
      in (Logic.ifElse (Regex.matches Constants.regexPackageName (Packaging.unPackageName pname)) Nothing (Just (ErrorPackaging.InvalidPackageErrorInvalidPackageName (ErrorPackaging.InvalidPackageNameError {
        ErrorPackaging.invalidPackageNameErrorPackageName = pname}))))
-- | Extract the name from a definition
definitionName :: Packaging.Definition -> Core.Name
definitionName def =
    case def of
      Packaging.DefinitionTerm v0 -> Packaging.termDefinitionName v0
      Packaging.DefinitionType v0 -> Packaging.typeDefinitionName v0
-- | True iff the given rule name appears in the profile's errorRules or warningRules.
enabledPackaging :: Validation.ValidationProfile -> Core.Name -> Bool
enabledPackaging p ruleName =
    Logic.or (Sets.member ruleName (Validation.validationProfileErrorRules p)) (Sets.member ruleName (Validation.validationProfileWarningRules p))
-- | The default validation profile for module/package validation. Every kernel-shipped check classified as an error; no warnings; maxErrors=1, maxWarnings=20.
kernelDefaultPackagingProfile :: Validation.ValidationProfile
kernelDefaultPackagingProfile =
    Validation.ValidationProfile {
      Validation.validationProfileErrorRules = (Sets.fromList [
        Core.Name "hydra.error.packaging.InvalidModuleError.conflictingVariantName",
        (Core.Name "hydra.error.packaging.InvalidModuleError.definitionNotInModuleName"),
        (Core.Name "hydra.error.packaging.InvalidModuleError.definitionsOutOfOrder"),
        (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName"),
        (Core.Name "hydra.error.packaging.InvalidModuleError.invalidDefinitionName"),
        (Core.Name "hydra.error.packaging.InvalidModuleError.invalidModuleNameConvention"),
        (Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation"),
        (Core.Name "hydra.error.packaging.InvalidPackageError.conflictingModuleName"),
        (Core.Name "hydra.error.packaging.InvalidPackageError.duplicateModuleName"),
        (Core.Name "hydra.error.packaging.InvalidPackageError.invalidPackageName")]),
      Validation.validationProfileWarningRules = Sets.empty,
      Validation.validationProfileMaxErrors = 1,
      Validation.validationProfileMaxWarnings = 20}
-- | Validate a kernel module against all kernel-default packaging rules; returns the first error found or nothing if valid. Convenience wrapper around 'module'' with 'kernelDefaultPackagingProfile'.
kernelModule :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
kernelModule mod =
    Lists.maybeHead (Validation.validationResultErrors (module_ kernelDefaultPackagingProfile (Validation.ValidationResult {
      Validation.validationResultErrors = [],
      Validation.validationResultWarnings = []}) mod))
-- | Validate a kernel package against all kernel-default packaging rules; returns the first error found or nothing if valid. Convenience wrapper around 'package' with 'kernelDefaultPackagingProfile'.
kernelPackage :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
kernelPackage pkg =
    Lists.maybeHead (Validation.validationResultErrors (package kernelDefaultPackagingProfile (Validation.ValidationResult {
      Validation.validationResultErrors = [],
      Validation.validationResultWarnings = []}) pkg))
-- | Validate a module against the given ValidationProfile, accumulating findings into a ValidationResult. Errors hard-stop the rule sequence once maxErrors is reached.
module_ :: Validation.ValidationProfile -> Validation.ValidationResult ErrorPackaging.InvalidModuleError -> Packaging.Module -> Validation.ValidationResult ErrorPackaging.InvalidModuleError
module_ p acc0 mod =
    Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingModule p acc guarded)) acc0 [
      Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.conflictingVariantName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.conflictingVariantName", f)) (checkConflictingVariantNames mod)) Nothing,
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.missingDocumentation", f)) (checkDefinitionDocumentation mod)) Nothing),
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.invalidDefinitionName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.invalidDefinitionName", f)) (checkDefinitionNameConvention mod)) Nothing),
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.definitionNotInModuleName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.definitionNotInModuleName", f)) (checkDefinitionModuleNames mod)) Nothing),
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.definitionsOutOfOrder")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.definitionsOutOfOrder", f)) (checkDefinitionOrdering mod)) Nothing),
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.duplicateDefinitionName", f)) (checkDuplicateDefinitionNames mod)) Nothing),
      (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidModuleError.invalidModuleNameConvention")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidModuleError.invalidModuleNameConvention", f)) (checkModuleNameConvention mod)) Nothing)]
-- | Validate a package against the given ValidationProfile, accumulating findings into a ValidationResult. Errors hard-stop traversal once maxErrors is reached.
package :: Validation.ValidationProfile -> Validation.ValidationResult ErrorPackaging.InvalidPackageError -> Packaging.Package -> Validation.ValidationResult ErrorPackaging.InvalidPackageError
package p acc0 pkg =

      let accPkg =
              Lists.foldl (\acc -> \guarded -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (appendFindingPackage p acc guarded)) acc0 [
                Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidPackageError.conflictingModuleName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidPackageError.conflictingModuleName", f)) (checkConflictingModuleNames pkg)) Nothing,
                (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidPackageError.duplicateModuleName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidPackageError.duplicateModuleName", f)) (checkDuplicateModuleNames pkg)) Nothing),
                (Logic.ifElse (enabledPackaging p (Core.Name "hydra.error.packaging.InvalidPackageError.invalidPackageName")) (Maybes.map (\f -> (Core.Name "hydra.error.packaging.InvalidPackageError.invalidPackageName", f)) (checkPackageNameConvention pkg)) Nothing)]
      in (Lists.foldl (\acc -> \mod -> Logic.ifElse (Equality.gte (Lists.length (Validation.validationResultErrors acc)) (Validation.validationProfileMaxErrors p)) acc (
        let mr =
                module_ p (Validation.ValidationResult {
                  Validation.validationResultErrors = [],
                  Validation.validationResultWarnings = []}) mod
            liftedErrs = Lists.map (\e -> ErrorPackaging.InvalidPackageErrorInvalidModule e) (Validation.validationResultErrors mr)
            liftedWrns = Lists.map (\w -> ErrorPackaging.InvalidPackageErrorInvalidModule w) (Validation.validationResultWarnings mr)
            newErrs =
                    Lists.take (Validation.validationProfileMaxErrors p) (Lists.concat2 (Validation.validationResultErrors acc) liftedErrs)
            newWrns =
                    Lists.take (Validation.validationProfileMaxWarnings p) (Lists.concat2 (Validation.validationResultWarnings acc) liftedWrns)
        in Validation.ValidationResult {
          Validation.validationResultErrors = newErrs,
          Validation.validationResultWarnings = newWrns})) accPkg (Packaging.packageModules pkg))
