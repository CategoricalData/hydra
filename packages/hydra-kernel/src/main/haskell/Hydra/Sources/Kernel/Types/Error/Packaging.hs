module Hydra.Sources.Kernel.Types.Error.Packaging where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging


ns :: Namespace
ns = Namespace "hydra.error.packaging"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [Core.ns, Packaging.ns],
            moduleTypeDependencies = [Core.ns, Packaging.ns],
            moduleDescription = Just "Error types for module and package validation"}
  where
    definitions = [
      conflictingModuleNamespaceError,
      conflictingVariantNameError,
      definitionNotInModuleNamespaceError,
      duplicateDefinitionNameError,
      duplicateModuleNamespaceError,
      invalidModuleError,
      invalidPackageError]

conflictingModuleNamespaceError :: Binding
conflictingModuleNamespaceError = define "ConflictingModuleNamespaceError" $
  doc "A module namespace which, when mapped to a target language's directory or package structure, conflicts with another module's mapped namespace. For example, hydra.foo.bar and hydra.fooBar might both map to the same directory in a case-insensitive filesystem." $
  T.record [
    "first">:
      doc "The first module namespace" $
      Packaging.namespace,
    "second">:
      doc "The second module namespace that conflicts with the first" $
      Packaging.namespace]

conflictingVariantNameError :: Binding
conflictingVariantNameError = define "ConflictingVariantNameError" $
  doc "A union type variant name which, when capitalized and concatenated with its type name, conflicts with another type definition name. For example, a union type Foo with a variant bar produces FooBar, which conflicts with an existing type definition FooBar. This is currently a problem only for the Haskell target." $
  T.record [
    "namespace">:
      doc "The namespace of the module containing the conflict" $
      Packaging.namespace,
    "typeName">:
      doc "The name of the union type" $
      Core.name,
    "variantName">:
      doc "The name of the variant field causing the conflict" $
      Core.name,
    "conflictingName">:
      doc "The name of the other type definition that conflicts with the generated constructor name" $
      Core.name]

definitionNotInModuleNamespaceError :: Binding
definitionNotInModuleNamespaceError = define "DefinitionNotInModuleNamespaceError" $
  doc "A definition whose name does not have the module's namespace as a prefix. If the module namespace is foo.bar, all definition names must have the form foo.bar.quux." $
  T.record [
    "namespace">:
      doc "The namespace of the module" $
      Packaging.namespace,
    "name">:
      doc "The definition name that does not match the module namespace" $
      Core.name]

duplicateDefinitionNameError :: Binding
duplicateDefinitionNameError = define "DuplicateDefinitionNameError" $
  doc "Two or more definitions in the same module share the same name" $
  T.record [
    "namespace">:
      doc "The namespace of the module containing the duplicates" $
      Packaging.namespace,
    "name">:
      doc "The duplicated definition name" $
      Core.name]

duplicateModuleNamespaceError :: Binding
duplicateModuleNamespaceError = define "DuplicateModuleNamespaceError" $
  doc "Two or more modules in the same package share the same namespace" $
  T.record [
    "namespace">:
      doc "The duplicated module namespace" $
      Packaging.namespace]

invalidModuleError :: Binding
invalidModuleError = define "InvalidModuleError" $
  doc "An error indicating that a module is invalid" $
  T.union [
    "conflictingVariantName">:
      doc "A union variant name that conflicts with another type definition when mapped to a target language" $
      conflictingVariantNameError,
    "definitionNotInModuleNamespace">:
      doc "A definition whose name does not have the module's namespace as a prefix" $
      definitionNotInModuleNamespaceError,
    "duplicateDefinitionName">:
      doc "Two or more definitions in the same module share the same name" $
      duplicateDefinitionNameError]

invalidPackageError :: Binding
invalidPackageError = define "InvalidPackageError" $
  doc "An error indicating that a package is invalid" $
  T.union [
    "conflictingModuleNamespace">:
      doc "Two module namespaces that conflict when mapped to a target language" $
      conflictingModuleNamespaceError,
    "duplicateModuleNamespace">:
      doc "Two or more modules in the same package share the same namespace" $
      duplicateModuleNamespaceError,
    "invalidModule">:
      doc "A module within the package is invalid" $
      invalidModuleError]
