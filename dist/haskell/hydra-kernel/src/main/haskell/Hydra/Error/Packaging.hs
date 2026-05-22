-- Note: this is an automatically generated file. Do not edit.
-- | Error types for module and package validation

module Hydra.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A module name which, when mapped to a target language's directory or package structure, conflicts with another module's mapped name. For example, hydra.foo.bar and hydra.fooBar might both map to the same directory in a case-insensitive filesystem.
data ConflictingModuleNameError =
  ConflictingModuleNameError {
    -- | The first module name
    conflictingModuleNameErrorFirst :: Packaging.ModuleName,
    -- | The second module name that conflicts with the first
    conflictingModuleNameErrorSecond :: Packaging.ModuleName}
  deriving (Eq, Ord, Read, Show)
_ConflictingModuleNameError = Core.Name "hydra.error.packaging.ConflictingModuleNameError"
_ConflictingModuleNameError_first = Core.Name "first"
_ConflictingModuleNameError_second = Core.Name "second"
-- | A union type variant name which, when capitalized and concatenated with its type name, conflicts with another type definition name. For example, a union type Foo with a variant bar produces FooBar, which conflicts with an existing type definition FooBar. This is currently a problem only for the Haskell target.
data ConflictingVariantNameError =
  ConflictingVariantNameError {
    -- | The name of the module containing the conflict
    conflictingVariantNameErrorModuleName :: Packaging.ModuleName,
    -- | The name of the union type
    conflictingVariantNameErrorTypeName :: Core.Name,
    -- | The name of the variant field causing the conflict
    conflictingVariantNameErrorVariantName :: Core.Name,
    -- | The name of the other type definition that conflicts with the generated constructor name
    conflictingVariantNameErrorConflictingName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_ConflictingVariantNameError = Core.Name "hydra.error.packaging.ConflictingVariantNameError"
_ConflictingVariantNameError_moduleName = Core.Name "moduleName"
_ConflictingVariantNameError_typeName = Core.Name "typeName"
_ConflictingVariantNameError_variantName = Core.Name "variantName"
_ConflictingVariantNameError_conflictingName = Core.Name "conflictingName"
-- | A definition whose name does not have the module's name as a prefix. If the module name is foo.bar, all definition names must have the form foo.bar.quux.
data DefinitionNotInModuleNameError =
  DefinitionNotInModuleNameError {
    -- | The name of the module
    definitionNotInModuleNameErrorModuleName :: Packaging.ModuleName,
    -- | The definition name that does not match the module name
    definitionNotInModuleNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DefinitionNotInModuleNameError = Core.Name "hydra.error.packaging.DefinitionNotInModuleNameError"
_DefinitionNotInModuleNameError_moduleName = Core.Name "moduleName"
_DefinitionNotInModuleNameError_name = Core.Name "name"
-- | Two consecutive definitions in a module's definitions list that are not in alphabetical order by local name. Hydra requires definitions to appear in lexicographic ASCII order of their local names.
data DefinitionsOutOfOrderError =
  DefinitionsOutOfOrderError {
    -- | The name of the module containing the misordered definitions
    definitionsOutOfOrderErrorModuleName :: Packaging.ModuleName,
    -- | The definition that appears first in the list
    definitionsOutOfOrderErrorPrecedingName :: Core.Name,
    -- | The definition that appears next, but should sort earlier than precedingName
    definitionsOutOfOrderErrorFollowingName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DefinitionsOutOfOrderError = Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"
_DefinitionsOutOfOrderError_moduleName = Core.Name "moduleName"
_DefinitionsOutOfOrderError_precedingName = Core.Name "precedingName"
_DefinitionsOutOfOrderError_followingName = Core.Name "followingName"
-- | Two or more definitions in the same module share the same name
data DuplicateDefinitionNameError =
  DuplicateDefinitionNameError {
    -- | The name of the module containing the duplicates
    duplicateDefinitionNameErrorModuleName :: Packaging.ModuleName,
    -- | The duplicated definition name
    duplicateDefinitionNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DuplicateDefinitionNameError = Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"
_DuplicateDefinitionNameError_moduleName = Core.Name "moduleName"
_DuplicateDefinitionNameError_name = Core.Name "name"
-- | Two or more modules in the same package share the same name
data DuplicateModuleNameError =
  DuplicateModuleNameError {
    -- | The duplicated module name
    duplicateModuleNameErrorModuleName :: Packaging.ModuleName}
  deriving (Eq, Ord, Read, Show)
_DuplicateModuleNameError = Core.Name "hydra.error.packaging.DuplicateModuleNameError"
_DuplicateModuleNameError_moduleName = Core.Name "moduleName"
-- | A definition whose local name does not match the expected naming convention. Term-level definitions must be camelCase; type-level definitions must be PascalCase.
data InvalidDefinitionNameError =
  InvalidDefinitionNameError {
    -- | The name of the module containing the definition
    invalidDefinitionNameErrorModuleName :: Packaging.ModuleName,
    -- | The definition name that violates the naming convention
    invalidDefinitionNameErrorName :: Core.Name,
    -- | The case convention the name should have followed (camel for term-level, pascal for type-level)
    invalidDefinitionNameErrorExpectedConvention :: Util.CaseConvention}
  deriving (Eq, Ord, Read, Show)
_InvalidDefinitionNameError = Core.Name "hydra.error.packaging.InvalidDefinitionNameError"
_InvalidDefinitionNameError_moduleName = Core.Name "moduleName"
_InvalidDefinitionNameError_name = Core.Name "name"
_InvalidDefinitionNameError_expectedConvention = Core.Name "expectedConvention"
-- | An error indicating that a module is invalid
data InvalidModuleError =
  -- | A union variant name that conflicts with another type definition when mapped to a target language
  InvalidModuleErrorConflictingVariantName ConflictingVariantNameError |
  -- | A definition whose name does not have the module's name as a prefix
  InvalidModuleErrorDefinitionNotInModuleName DefinitionNotInModuleNameError |
  -- | Two consecutive definitions in the definitions list that are not in alphabetical order
  InvalidModuleErrorDefinitionsOutOfOrder DefinitionsOutOfOrderError |
  -- | Two or more definitions in the same module share the same name
  InvalidModuleErrorDuplicateDefinitionName DuplicateDefinitionNameError |
  -- | A definition whose local name does not match the expected naming convention
  InvalidModuleErrorInvalidDefinitionName InvalidDefinitionNameError |
  -- | A module whose name does not match the module-name naming convention
  InvalidModuleErrorInvalidModuleNameConvention InvalidModuleNameConventionError |
  -- | A top-level definition lacking a description annotation
  InvalidModuleErrorMissingDocumentation MissingDocumentationError
  deriving (Eq, Ord, Read, Show)
_InvalidModuleError = Core.Name "hydra.error.packaging.InvalidModuleError"
_InvalidModuleError_conflictingVariantName = Core.Name "conflictingVariantName"
_InvalidModuleError_definitionNotInModuleName = Core.Name "definitionNotInModuleName"
_InvalidModuleError_definitionsOutOfOrder = Core.Name "definitionsOutOfOrder"
_InvalidModuleError_duplicateDefinitionName = Core.Name "duplicateDefinitionName"
_InvalidModuleError_invalidDefinitionName = Core.Name "invalidDefinitionName"
_InvalidModuleError_invalidModuleNameConvention = Core.Name "invalidModuleNameConvention"
_InvalidModuleError_missingDocumentation = Core.Name "missingDocumentation"
-- | A module whose name does not match the dotted-lowercase naming convention. Module names must be dot-separated lowercase segments, each starting with a letter, e.g. hydra.core or hydra.lib.lists.
data InvalidModuleNameConventionError =
  InvalidModuleNameConventionError {
    -- | The module name that violates the convention
    invalidModuleNameConventionErrorModuleName :: Packaging.ModuleName}
  deriving (Eq, Ord, Read, Show)
_InvalidModuleNameConventionError = Core.Name "hydra.error.packaging.InvalidModuleNameConventionError"
_InvalidModuleNameConventionError_moduleName = Core.Name "moduleName"
-- | An error indicating that a package is invalid
data InvalidPackageError =
  -- | Two module names that conflict when mapped to a target language
  InvalidPackageErrorConflictingModuleName ConflictingModuleNameError |
  -- | Two or more modules in the same package share the same name
  InvalidPackageErrorDuplicateModuleName DuplicateModuleNameError |
  -- | A module within the package is invalid
  InvalidPackageErrorInvalidModule InvalidModuleError |
  -- | A package whose name does not match the package-name naming convention
  InvalidPackageErrorInvalidPackageName InvalidPackageNameError
  deriving (Eq, Ord, Read, Show)
_InvalidPackageError = Core.Name "hydra.error.packaging.InvalidPackageError"
_InvalidPackageError_conflictingModuleName = Core.Name "conflictingModuleName"
_InvalidPackageError_duplicateModuleName = Core.Name "duplicateModuleName"
_InvalidPackageError_invalidModule = Core.Name "invalidModule"
_InvalidPackageError_invalidPackageName = Core.Name "invalidPackageName"
-- | A package whose name does not match the hyphen-separated lowercase naming convention. Package names must be hyphen-separated lowercase segments, each starting with a letter, e.g. hydra-kernel or hydra-python.
data InvalidPackageNameError =
  InvalidPackageNameError {
    -- | The package name that violates the convention
    invalidPackageNameErrorPackageName :: Packaging.PackageName}
  deriving (Eq, Ord, Read, Show)
_InvalidPackageNameError = Core.Name "hydra.error.packaging.InvalidPackageNameError"
_InvalidPackageNameError_packageName = Core.Name "packageName"
-- | A top-level definition whose term (or type, for type definitions) lacks a description annotation. Every definition in a Hydra module is expected to be wrapped in a doc "..." annotation at its top level.
data MissingDocumentationError =
  MissingDocumentationError {
    -- | The name of the module containing the undocumented definition
    missingDocumentationErrorModuleName :: Packaging.ModuleName,
    -- | The name of the undocumented definition
    missingDocumentationErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_MissingDocumentationError = Core.Name "hydra.error.packaging.MissingDocumentationError"
_MissingDocumentationError_moduleName = Core.Name "moduleName"
_MissingDocumentationError_name = Core.Name "name"
