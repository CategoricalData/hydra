-- Note: this is an automatically generated file. Do not edit.
-- | Error types for module and package validation

module Hydra.Error.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A module namespace which, when mapped to a target language's directory or package structure, conflicts with another module's mapped namespace. For example, hydra.foo.bar and hydra.fooBar might both map to the same directory in a case-insensitive filesystem.
data ConflictingModuleNamespaceError =
  ConflictingModuleNamespaceError {
    -- | The first module namespace
    conflictingModuleNamespaceErrorFirst :: Packaging.Namespace,
    -- | The second module namespace that conflicts with the first
    conflictingModuleNamespaceErrorSecond :: Packaging.Namespace}
  deriving (Eq, Ord, Read, Show)
_ConflictingModuleNamespaceError = Core.Name "hydra.error.packaging.ConflictingModuleNamespaceError"
_ConflictingModuleNamespaceError_first = Core.Name "first"
_ConflictingModuleNamespaceError_second = Core.Name "second"
-- | A union type variant name which, when capitalized and concatenated with its type name, conflicts with another type definition name. For example, a union type Foo with a variant bar produces FooBar, which conflicts with an existing type definition FooBar. This is currently a problem only for the Haskell target.
data ConflictingVariantNameError =
  ConflictingVariantNameError {
    -- | The namespace of the module containing the conflict
    conflictingVariantNameErrorNamespace :: Packaging.Namespace,
    -- | The name of the union type
    conflictingVariantNameErrorTypeName :: Core.Name,
    -- | The name of the variant field causing the conflict
    conflictingVariantNameErrorVariantName :: Core.Name,
    -- | The name of the other type definition that conflicts with the generated constructor name
    conflictingVariantNameErrorConflictingName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_ConflictingVariantNameError = Core.Name "hydra.error.packaging.ConflictingVariantNameError"
_ConflictingVariantNameError_namespace = Core.Name "namespace"
_ConflictingVariantNameError_typeName = Core.Name "typeName"
_ConflictingVariantNameError_variantName = Core.Name "variantName"
_ConflictingVariantNameError_conflictingName = Core.Name "conflictingName"
-- | A definition whose name does not have the module's namespace as a prefix. If the module namespace is foo.bar, all definition names must have the form foo.bar.quux.
data DefinitionNotInModuleNamespaceError =
  DefinitionNotInModuleNamespaceError {
    -- | The namespace of the module
    definitionNotInModuleNamespaceErrorNamespace :: Packaging.Namespace,
    -- | The definition name that does not match the module namespace
    definitionNotInModuleNamespaceErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DefinitionNotInModuleNamespaceError = Core.Name "hydra.error.packaging.DefinitionNotInModuleNamespaceError"
_DefinitionNotInModuleNamespaceError_namespace = Core.Name "namespace"
_DefinitionNotInModuleNamespaceError_name = Core.Name "name"
-- | Two consecutive definitions in a module's definitions list that are not in alphabetical order by local name. Hydra requires definitions to appear in lexicographic ASCII order of their local names.
data DefinitionsOutOfOrderError =
  DefinitionsOutOfOrderError {
    -- | The namespace of the module containing the misordered definitions
    definitionsOutOfOrderErrorNamespace :: Packaging.Namespace,
    -- | The definition that appears first in the list
    definitionsOutOfOrderErrorPrecedingName :: Core.Name,
    -- | The definition that appears next, but should sort earlier than precedingName
    definitionsOutOfOrderErrorFollowingName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DefinitionsOutOfOrderError = Core.Name "hydra.error.packaging.DefinitionsOutOfOrderError"
_DefinitionsOutOfOrderError_namespace = Core.Name "namespace"
_DefinitionsOutOfOrderError_precedingName = Core.Name "precedingName"
_DefinitionsOutOfOrderError_followingName = Core.Name "followingName"
-- | Two or more definitions in the same module share the same name
data DuplicateDefinitionNameError =
  DuplicateDefinitionNameError {
    -- | The namespace of the module containing the duplicates
    duplicateDefinitionNameErrorNamespace :: Packaging.Namespace,
    -- | The duplicated definition name
    duplicateDefinitionNameErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_DuplicateDefinitionNameError = Core.Name "hydra.error.packaging.DuplicateDefinitionNameError"
_DuplicateDefinitionNameError_namespace = Core.Name "namespace"
_DuplicateDefinitionNameError_name = Core.Name "name"
-- | Two or more modules in the same package share the same namespace
data DuplicateModuleNamespaceError =
  DuplicateModuleNamespaceError {
    -- | The duplicated module namespace
    duplicateModuleNamespaceErrorNamespace :: Packaging.Namespace}
  deriving (Eq, Ord, Read, Show)
_DuplicateModuleNamespaceError = Core.Name "hydra.error.packaging.DuplicateModuleNamespaceError"
_DuplicateModuleNamespaceError_namespace = Core.Name "namespace"
-- | A definition whose local name does not match the expected naming convention. Term-level definitions must be camelCase; type-level definitions must be PascalCase.
data InvalidDefinitionNameError =
  InvalidDefinitionNameError {
    -- | The namespace of the module containing the definition
    invalidDefinitionNameErrorNamespace :: Packaging.Namespace,
    -- | The definition name that violates the naming convention
    invalidDefinitionNameErrorName :: Core.Name,
    -- | The case convention the name should have followed (camel for term-level, pascal for type-level)
    invalidDefinitionNameErrorExpectedConvention :: Util.CaseConvention}
  deriving (Eq, Ord, Read, Show)
_InvalidDefinitionNameError = Core.Name "hydra.error.packaging.InvalidDefinitionNameError"
_InvalidDefinitionNameError_namespace = Core.Name "namespace"
_InvalidDefinitionNameError_name = Core.Name "name"
_InvalidDefinitionNameError_expectedConvention = Core.Name "expectedConvention"
-- | An error indicating that a module is invalid
data InvalidModuleError =
  -- | A union variant name that conflicts with another type definition when mapped to a target language
  InvalidModuleErrorConflictingVariantName ConflictingVariantNameError |
  -- | A definition whose name does not have the module's namespace as a prefix
  InvalidModuleErrorDefinitionNotInModuleNamespace DefinitionNotInModuleNamespaceError |
  -- | Two consecutive definitions in the definitions list that are not in alphabetical order
  InvalidModuleErrorDefinitionsOutOfOrder DefinitionsOutOfOrderError |
  -- | Two or more definitions in the same module share the same name
  InvalidModuleErrorDuplicateDefinitionName DuplicateDefinitionNameError |
  -- | A definition whose local name does not match the expected naming convention
  InvalidModuleErrorInvalidDefinitionName InvalidDefinitionNameError |
  -- | A module whose namespace does not match the namespace naming convention
  InvalidModuleErrorInvalidNamespaceConvention InvalidNamespaceConventionError |
  -- | A top-level definition lacking a description annotation
  InvalidModuleErrorMissingDocumentation MissingDocumentationError
  deriving (Eq, Ord, Read, Show)
_InvalidModuleError = Core.Name "hydra.error.packaging.InvalidModuleError"
_InvalidModuleError_conflictingVariantName = Core.Name "conflictingVariantName"
_InvalidModuleError_definitionNotInModuleNamespace = Core.Name "definitionNotInModuleNamespace"
_InvalidModuleError_definitionsOutOfOrder = Core.Name "definitionsOutOfOrder"
_InvalidModuleError_duplicateDefinitionName = Core.Name "duplicateDefinitionName"
_InvalidModuleError_invalidDefinitionName = Core.Name "invalidDefinitionName"
_InvalidModuleError_invalidNamespaceConvention = Core.Name "invalidNamespaceConvention"
_InvalidModuleError_missingDocumentation = Core.Name "missingDocumentation"
-- | A module whose namespace does not match the dotted-lowercase naming convention. Namespaces must be dot-separated lowercase segments, each starting with a letter, e.g. hydra.core or hydra.lib.lists.
data InvalidNamespaceConventionError =
  InvalidNamespaceConventionError {
    -- | The namespace that violates the convention
    invalidNamespaceConventionErrorNamespace :: Packaging.Namespace}
  deriving (Eq, Ord, Read, Show)
_InvalidNamespaceConventionError = Core.Name "hydra.error.packaging.InvalidNamespaceConventionError"
_InvalidNamespaceConventionError_namespace = Core.Name "namespace"
-- | An error indicating that a package is invalid
data InvalidPackageError =
  -- | Two module namespaces that conflict when mapped to a target language
  InvalidPackageErrorConflictingModuleNamespace ConflictingModuleNamespaceError |
  -- | Two or more modules in the same package share the same namespace
  InvalidPackageErrorDuplicateModuleNamespace DuplicateModuleNamespaceError |
  -- | A module within the package is invalid
  InvalidPackageErrorInvalidModule InvalidModuleError |
  -- | A package whose name does not match the package-name naming convention
  InvalidPackageErrorInvalidPackageName InvalidPackageNameError
  deriving (Eq, Ord, Read, Show)
_InvalidPackageError = Core.Name "hydra.error.packaging.InvalidPackageError"
_InvalidPackageError_conflictingModuleNamespace = Core.Name "conflictingModuleNamespace"
_InvalidPackageError_duplicateModuleNamespace = Core.Name "duplicateModuleNamespace"
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
    -- | The namespace of the module containing the undocumented definition
    missingDocumentationErrorNamespace :: Packaging.Namespace,
    -- | The name of the undocumented definition
    missingDocumentationErrorName :: Core.Name}
  deriving (Eq, Ord, Read, Show)
_MissingDocumentationError = Core.Name "hydra.error.packaging.MissingDocumentationError"
_MissingDocumentationError_namespace = Core.Name "namespace"
_MissingDocumentationError_name = Core.Name "name"
