-- Note: this is an automatically generated file. Do not edit.

-- | Error types for module and package validation

module Hydra.Error.Packaging where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

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

-- | An error indicating that a module is invalid
data InvalidModuleError =
  -- | A union variant name that conflicts with another type definition when mapped to a target language
  InvalidModuleErrorConflictingVariantName ConflictingVariantNameError |
  -- | A definition whose name does not have the module's namespace as a prefix
  InvalidModuleErrorDefinitionNotInModuleNamespace DefinitionNotInModuleNamespaceError |
  -- | Two or more definitions in the same module share the same name
  InvalidModuleErrorDuplicateDefinitionName DuplicateDefinitionNameError
  deriving (Eq, Ord, Read, Show)

_InvalidModuleError = Core.Name "hydra.error.packaging.InvalidModuleError"

_InvalidModuleError_conflictingVariantName = Core.Name "conflictingVariantName"

_InvalidModuleError_definitionNotInModuleNamespace = Core.Name "definitionNotInModuleNamespace"

_InvalidModuleError_duplicateDefinitionName = Core.Name "duplicateDefinitionName"

-- | An error indicating that a package is invalid
data InvalidPackageError =
  -- | Two module namespaces that conflict when mapped to a target language
  InvalidPackageErrorConflictingModuleNamespace ConflictingModuleNamespaceError |
  -- | Two or more modules in the same package share the same namespace
  InvalidPackageErrorDuplicateModuleNamespace DuplicateModuleNamespaceError |
  -- | A module within the package is invalid
  InvalidPackageErrorInvalidModule InvalidModuleError
  deriving (Eq, Ord, Read, Show)

_InvalidPackageError = Core.Name "hydra.error.packaging.InvalidPackageError"

_InvalidPackageError_conflictingModuleNamespace = Core.Name "conflictingModuleNamespace"

_InvalidPackageError_duplicateModuleNamespace = Core.Name "duplicateModuleNamespace"

_InvalidPackageError_invalidModule = Core.Name "invalidModule"
