-- Note: this is an automatically generated file. Do not edit.
-- | String representations of hydra.error.packaging types

module Hydra.Show.Error.Packaging where
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Show.Util as ShowUtil
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Show a conflicting module namespace error as a string
conflictingModuleNameError :: ErrorPackaging.ConflictingModuleNameError -> String
conflictingModuleNameError e =
    Strings.cat [
      "module namespaces conflict when mapped to target language path: ",
      (Packaging.unModuleName (ErrorPackaging.conflictingModuleNameErrorFirst e)),
      " vs ",
      (Packaging.unModuleName (ErrorPackaging.conflictingModuleNameErrorSecond e))]
-- | Show a conflicting variant name error as a string
conflictingVariantNameError :: ErrorPackaging.ConflictingVariantNameError -> String
conflictingVariantNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unModuleName (ErrorPackaging.conflictingVariantNameErrorModuleName e)),
      ": variant ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorVariantName e)),
      " of type ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorTypeName e)),
      " produces constructor name ",
      (Core.unName (ErrorPackaging.conflictingVariantNameErrorConflictingName e)),
      " which conflicts with another type definition"]
-- | Show a definition-not-in-module-namespace error as a string
definitionNotInModuleNameError :: ErrorPackaging.DefinitionNotInModuleNameError -> String
definitionNotInModuleNameError e =
    Strings.cat [
      "definition ",
      (Core.unName (ErrorPackaging.definitionNotInModuleNameErrorName e)),
      " is not in module namespace ",
      (Packaging.unModuleName (ErrorPackaging.definitionNotInModuleNameErrorModuleName e))]
-- | Show a definitions-out-of-order error as a string
definitionsOutOfOrderError :: ErrorPackaging.DefinitionsOutOfOrderError -> String
definitionsOutOfOrderError e =
    Strings.cat [
      "in module ",
      (Packaging.unModuleName (ErrorPackaging.definitionsOutOfOrderErrorModuleName e)),
      ": definitions out of order: ",
      (Core.unName (ErrorPackaging.definitionsOutOfOrderErrorPrecedingName e)),
      " precedes ",
      (Core.unName (ErrorPackaging.definitionsOutOfOrderErrorFollowingName e))]
-- | Show a duplicate definition name error as a string
duplicateDefinitionNameError :: ErrorPackaging.DuplicateDefinitionNameError -> String
duplicateDefinitionNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unModuleName (ErrorPackaging.duplicateDefinitionNameErrorModuleName e)),
      ": duplicate definition name ",
      (Core.unName (ErrorPackaging.duplicateDefinitionNameErrorName e))]
-- | Show a duplicate module namespace error as a string
duplicateModuleNameError :: ErrorPackaging.DuplicateModuleNameError -> String
duplicateModuleNameError e =
    Strings.cat [
      "duplicate module namespace ",
      (Packaging.unModuleName (ErrorPackaging.duplicateModuleNameErrorModuleName e))]
-- | Show an invalid definition name error as a string
invalidDefinitionNameError :: ErrorPackaging.InvalidDefinitionNameError -> String
invalidDefinitionNameError e =
    Strings.cat [
      "in module ",
      (Packaging.unModuleName (ErrorPackaging.invalidDefinitionNameErrorModuleName e)),
      ": definition ",
      (Core.unName (ErrorPackaging.invalidDefinitionNameErrorName e)),
      " does not match expected ",
      (ShowUtil.caseConvention (ErrorPackaging.invalidDefinitionNameErrorExpectedConvention e))]
-- | Show an invalid module error as a string
invalidModuleError :: ErrorPackaging.InvalidModuleError -> String
invalidModuleError e =
    Strings.cat2 "invalid module: " (case e of
      ErrorPackaging.InvalidModuleErrorConflictingVariantName v0 -> conflictingVariantNameError v0
      ErrorPackaging.InvalidModuleErrorDefinitionNotInModuleName v0 -> definitionNotInModuleNameError v0
      ErrorPackaging.InvalidModuleErrorDefinitionsOutOfOrder v0 -> definitionsOutOfOrderError v0
      ErrorPackaging.InvalidModuleErrorDuplicateDefinitionName v0 -> duplicateDefinitionNameError v0
      ErrorPackaging.InvalidModuleErrorInvalidDefinitionName v0 -> invalidDefinitionNameError v0
      ErrorPackaging.InvalidModuleErrorInvalidModuleNameConvention v0 -> invalidModuleNameConventionError v0
      ErrorPackaging.InvalidModuleErrorMissingDocumentation v0 -> missingDocumentationError v0)
-- | Show an invalid namespace convention error as a string
invalidModuleNameConventionError :: ErrorPackaging.InvalidModuleNameConventionError -> String
invalidModuleNameConventionError e =
    Strings.cat [
      "namespace ",
      (Packaging.unModuleName (ErrorPackaging.invalidModuleNameConventionErrorModuleName e)),
      " does not match the dotted-camelCase naming convention"]
-- | Show an invalid package error as a string
invalidPackageError :: ErrorPackaging.InvalidPackageError -> String
invalidPackageError e =
    Strings.cat2 "invalid package: " (case e of
      ErrorPackaging.InvalidPackageErrorConflictingModuleName v0 -> conflictingModuleNameError v0
      ErrorPackaging.InvalidPackageErrorDuplicateModuleName v0 -> duplicateModuleNameError v0
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
      (Packaging.unModuleName (ErrorPackaging.missingDocumentationErrorModuleName e)),
      ": definition ",
      (Core.unName (ErrorPackaging.missingDocumentationErrorName e)),
      " lacks a description annotation"]
