-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.error.packaging types

module Hydra.Show.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Show.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a conflicting module namespace error as a string
conflictingModuleNamespaceError :: ErrorPackaging.ConflictingModuleNamespaceError -> String
conflictingModuleNamespaceError e =
    Strings.cat [
      "module namespaces conflict when mapped to target language path: ",
      (Packaging.unNamespace (ErrorPackaging.conflictingModuleNamespaceErrorFirst e)),
      " vs ",
      (Packaging.unNamespace (ErrorPackaging.conflictingModuleNamespaceErrorSecond e))]
-- | Show a conflicting variant name error as a string
conflictingVariantNameError :: ErrorPackaging.ConflictingVariantNameError -> String
conflictingVariantNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unNamespace (ErrorPackaging.conflictingVariantNameErrorNamespace e)),
      ": variant ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorVariantName e)),
      " of type ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorTypeName e)),
      " produces constructor name ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorConflictingName e)),
      " which conflicts with another type definition"]
-- | Show a definition-not-in-module-namespace error as a string
definitionNotInModuleNamespaceError :: ErrorPackaging.DefinitionNotInModuleNamespaceError -> String
definitionNotInModuleNamespaceError e =
    Strings.cat [
      "definition ",
      (Core.unName (ErrorPackaging.definitionNotInModuleNamespaceErrorName e)),
      " is not in module namespace ",
      (Packaging.unNamespace (ErrorPackaging.definitionNotInModuleNamespaceErrorNamespace e))]
-- | Show a definitions-out-of-order error as a string
definitionsOutOfOrderError :: ErrorPackaging.DefinitionsOutOfOrderError -> String
definitionsOutOfOrderError e =
    Strings.cat [
      "in module ",
      (Packaging.unNamespace (ErrorPackaging.definitionsOutOfOrderErrorNamespace e)),
      ": definitions out of order: ",
      (Core.unName (ErrorPackaging.definitionsOutOfOrderErrorPrecedingName e)),
      " precedes ",
      (Core.unName (ErrorPackaging.definitionsOutOfOrderErrorFollowingName e))]
-- | Show a duplicate definition name error as a string
duplicateDefinitionNameError :: ErrorPackaging.DuplicateDefinitionNameError -> String
duplicateDefinitionNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unNamespace (ErrorPackaging.duplicateDefinitionNameErrorNamespace e)),
      ": duplicate definition name ",
      (Core.unName (ErrorPackaging.duplicateDefinitionNameErrorName e))]
-- | Show a duplicate module namespace error as a string
duplicateModuleNamespaceError :: ErrorPackaging.DuplicateModuleNamespaceError -> String
duplicateModuleNamespaceError e =
    Strings.cat [
      "duplicate module namespace ",
      (Packaging.unNamespace (ErrorPackaging.duplicateModuleNamespaceErrorNamespace e))]
-- | Show an invalid definition name error as a string
invalidDefinitionNameError :: ErrorPackaging.InvalidDefinitionNameError -> String
invalidDefinitionNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unNamespace (ErrorPackaging.invalidDefinitionNameErrorNamespace e)),
      ": definition ",
      (Core.unName (ErrorPackaging.invalidDefinitionNameErrorName e)),
      " does not match expected ",
      (Util.caseConvention (ErrorPackaging.invalidDefinitionNameErrorExpectedConvention e))]
-- | Show an invalid module error as a string
invalidModuleError :: ErrorPackaging.InvalidModuleError -> String
invalidModuleError e =
    Strings.cat2 "invalid module: " (case e of
      ErrorPackaging.InvalidModuleErrorConflictingVariantName v0 -> conflictingVariantNameError v0
      ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleNamespace v0 -> definitionNotInModuleNamespaceError v0
      ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder v0 -> definitionsOutOfOrderError v0
      ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName v0 -> duplicateDefinitionNameError v0
      ErrorPackaging.InvalidModuleErrorInvalidDefinitionName v0 -> invalidDefinitionNameError v0
      ErrorPackaging.InvalidModuleErrorInvalidNamespaceConvention v0 -> invalidNamespaceConventionError v0
      ErrorPackaging.InvalidModuleErrorMissingDocumentation v0 -> missingDocumentationError v0)
-- | Show an invalid namespace convention error as a string
invalidNamespaceConventionError :: ErrorPackaging.InvalidNamespaceConventionError -> String
invalidNamespaceConventionError e =
    Strings.cat [
      "namespace ",
      (Packaging.unNamespace (ErrorPackaging.invalidNamespaceConventionErrorNamespace e)),
      " does not match the dotted-camelCase naming convention"]
-- | Show an invalid package error as a string
invalidPackageError :: ErrorPackaging.InvalidPackageError -> String
invalidPackageError e =
    Strings.cat2 "invalid package: " (case e of
      ErrorPackaging.InvalidPackageErrorConflictingModuleNamespace v0 -> conflictingModuleNamespaceError v0
      ErrorPackaging.InvalidPackageErrorDuplicateModuleNamespace v0 -> duplicateModuleNamespaceError v0
      ErrorPackaging.InvalidPackageErrorInvalidModule v0 -> invalidModuleError v0
      ErrorPackaging.InvalidPackageErrorInvalidPackageName v0 -> invalidPackageNameError v0)
-- | Show an invalid package name error as a string
invalidPackageNameError :: ErrorPackaging.InvalidPackageNameError -> String
invalidPackageNameError e =
    Strings.cat [
      "package name ",
      (Packaging.unPackageName (ErrorPackaging.invalidPackageNameErrorPackageName e)),
      " does not match the hyphen-separated lowercase naming convention"]
-- | Show a missing documentation error as a string
missingDocumentationError :: ErrorPackaging.MissingDocumentationError -> String
missingDocumentationError e =
    Strings.cat [
      "in module ",
      (Packaging.unNamespace (ErrorPackaging.missingDocumentationErrorNamespace e)),
      ": definition ",
      (Core.unName (ErrorPackaging.missingDocumentationErrorName e)),
      " lacks a description annotation"]
