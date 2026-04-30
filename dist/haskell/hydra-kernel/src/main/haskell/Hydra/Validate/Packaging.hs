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
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Check for module namespaces that conflict when mapped to target language paths
checkConflictingModuleNamespaces :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
checkConflictingModuleNamespaces pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging.moduleNamespace mod
                      key = Strings.toLower (Packaging.unNamespace ns)
                      existing = Maps.lookup key seen
                  in (Maybes.cases existing (Maps.insert key ns seen, Nothing) (\first -> (
                    seen,
                    (Just (ErrorPackaging.InvalidPackageErrorConflictingModuleNamespace (ErrorPackaging.ConflictingModuleNamespaceError {
                      ErrorPackaging.conflictingModuleNamespaceErrorFirst = first,
                      ErrorPackaging.conflictingModuleNamespaceErrorSecond = ns}))))))) (\_ -> acc))) (Maps.empty, Nothing) (Packaging.packageModules pkg)
      in (Pairs.second result)
-- | Check for union variant names that, when mapped to constructor names, conflict with other type definitions
checkConflictingVariantNames :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkConflictingVariantNames mod =

      let ns = Packaging.moduleNamespace mod
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
                ErrorPackaging.conflictingVariantNameErrorNamespace = ns,
                ErrorPackaging.conflictingVariantNameErrorTypeName = typeName,
                ErrorPackaging.conflictingVariantNameErrorVariantName = fieldName,
                ErrorPackaging.conflictingVariantNameErrorConflictingName = (Core.Name constructorName)}))) Nothing)) (\_ -> innerAcc)) Nothing v1
            _ -> Nothing
        _ -> Nothing) (\_ -> acc)) Nothing defs)
-- | Check that every top-level definition is wrapped in a description annotation
checkDefinitionDocumentation :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionDocumentation mod =

      let ns = Packaging.moduleNamespace mod
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
          ErrorPackaging.missingDocumentationErrorNamespace = ns,
          ErrorPackaging.missingDocumentationErrorName = name}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that term definitions have camelCase local names and type definitions have PascalCase local names
checkDefinitionNameConvention :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionNameConvention mod =

      let ns = Packaging.moduleNamespace mod
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
          ErrorPackaging.invalidDefinitionNameErrorNamespace = ns,
          ErrorPackaging.invalidDefinitionNameErrorName = name,
          ErrorPackaging.invalidDefinitionNameErrorExpectedConvention = expected}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that all definition names in a module have the module's namespace as a prefix
checkDefinitionNamespaces :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionNamespaces mod =

      let ns = Packaging.moduleNamespace mod
          prefix = Strings.cat2 (Packaging.unNamespace ns) "."
          prefixLen = Strings.length prefix
      in (Lists.foldl (\acc -> \def -> Maybes.cases acc (
        let name = definitionName def
            nameStr = Core.unName name
            namePrefix = Lists.take prefixLen (Strings.toList nameStr)
        in (Logic.ifElse (Equality.equal (Strings.fromList namePrefix) prefix) Nothing (Just (ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleNamespace (ErrorPackaging.DefinitionNotInModuleNamespaceError {
          ErrorPackaging.definitionNotInModuleNamespaceErrorNamespace = ns,
          ErrorPackaging.definitionNotInModuleNamespaceErrorName = name}))))) (\_ -> acc)) Nothing (Packaging.moduleDefinitions mod))
-- | Check that a module's definitions list is sorted in ascending lexicographic order by local name
checkDefinitionOrdering :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDefinitionOrdering mod =

      let ns = Packaging.moduleNamespace mod
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
                            ErrorPackaging.definitionsOutOfOrderErrorNamespace = ns,
                            ErrorPackaging.definitionsOutOfOrderErrorPrecedingName = prevName,
                            ErrorPackaging.definitionsOutOfOrderErrorFollowingName = currName})))))))) (\_ -> acc))) (Nothing, Nothing) (Packaging.moduleDefinitions mod)
      in (Pairs.second result)
-- | Check for duplicate definition names in a module
checkDuplicateDefinitionNames :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkDuplicateDefinitionNames mod =

      let ns = Packaging.moduleNamespace mod
          result =
                  Lists.foldl (\acc -> \def ->
                    let seen = Pairs.first acc
                        err = Pairs.second acc
                    in (Maybes.cases err (
                      let name = definitionName def
                      in (Logic.ifElse (Sets.member name seen) (
                        seen,
                        (Just (ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName (ErrorPackaging.DuplicateDefinitionNameError {
                          ErrorPackaging.duplicateDefinitionNameErrorNamespace = ns,
                          ErrorPackaging.duplicateDefinitionNameErrorName = name})))) (Sets.insert name seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging.moduleDefinitions mod)
      in (Pairs.second result)
-- | Check for duplicate module namespaces in a package
checkDuplicateModuleNamespaces :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
checkDuplicateModuleNamespaces pkg =

      let result =
              Lists.foldl (\acc -> \mod ->
                let seen = Pairs.first acc
                    err = Pairs.second acc
                in (Maybes.cases err (
                  let ns = Packaging.moduleNamespace mod
                  in (Logic.ifElse (Sets.member ns seen) (
                    seen,
                    (Just (ErrorPackaging.InvalidPackageErrorDuplicateModuleNamespace (ErrorPackaging.DuplicateModuleNamespaceError {
                      ErrorPackaging.duplicateModuleNamespaceErrorNamespace = ns})))) (Sets.insert ns seen, Nothing))) (\_ -> acc))) (Sets.empty, Nothing) (Packaging.packageModules pkg)
      in (Pairs.second result)
-- | Check that the module's namespace matches the dotted-lowercase naming convention
checkModuleNamespaceConvention :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
checkModuleNamespaceConvention mod =

      let ns = Packaging.moduleNamespace mod
      in (Logic.ifElse (Regex.matches Constants.regexNamespace (Packaging.unNamespace ns)) Nothing (Just (ErrorPackaging.InvalidModuleErrorInvalidNamespaceConvention (ErrorPackaging.InvalidNamespaceConventionError {
        ErrorPackaging.invalidNamespaceConventionErrorNamespace = ns}))))
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
-- | Validate a kernel module against all packaging rules; returns the first error found or nothing if valid
kernelModule :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
kernelModule mod =
    Lists.foldl (\acc -> \check -> Maybes.cases acc (check mod) (\_ -> acc)) Nothing [
      checkConflictingVariantNames,
      checkDefinitionDocumentation,
      checkDefinitionNameConvention,
      checkDefinitionNamespaces,
      checkDefinitionOrdering,
      checkDuplicateDefinitionNames,
      checkModuleNamespaceConvention]
-- | Validate a kernel package against all packaging rules; returns the first error found or nothing if valid
kernelPackage :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
kernelPackage pkg =

      let pkgErr =
              Lists.foldl (\acc -> \check -> Maybes.cases acc (check pkg) (\_ -> acc)) Nothing [
                checkConflictingModuleNamespaces,
                checkDuplicateModuleNamespaces,
                checkPackageNameConvention]
      in (Maybes.cases pkgErr (Lists.foldl (\acc -> \mod -> Maybes.cases acc (Maybes.map (\err -> ErrorPackaging.InvalidPackageErrorInvalidModule err) (kernelModule mod)) (\_ -> acc)) Nothing (Packaging.packageModules pkg)) (\_ -> pkgErr))
-- | Validate a module against the structural rules; returns the first error found or nothing if valid
module_ :: Packaging.Module -> Maybe ErrorPackaging.InvalidModuleError
module_ mod =

      let r1 = checkDefinitionNamespaces mod
      in (Maybes.cases r1 (
        let r2 = checkDuplicateDefinitionNames mod
        in (Maybes.cases r2 (checkConflictingVariantNames mod) (\_ -> r2))) (\_ -> r1))
-- | Validate a package against the structural rules; returns the first error found or nothing if valid
package :: Packaging.Package -> Maybe ErrorPackaging.InvalidPackageError
package pkg =

      let r1 = checkDuplicateModuleNamespaces pkg
      in (Maybes.cases r1 (
        let r2 = checkConflictingModuleNamespaces pkg
        in (Maybes.cases r2 (Lists.foldl (\acc -> \mod -> Maybes.cases acc (Maybes.map (\err -> ErrorPackaging.InvalidPackageErrorInvalidModule err) (module_ mod)) (\_ -> acc)) Nothing (Packaging.packageModules pkg)) (\_ -> r2))) (\_ -> r1))
