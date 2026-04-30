module Hydra.Sources.Kernel.Types.Error.Packaging where

import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging
import qualified Hydra.Sources.Kernel.Types.Util as Util


ns :: Namespace
ns = Namespace "hydra.error.packaging"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleTermDependencies = [Core.ns, Packaging.ns, Util.ns],
            moduleTypeDependencies = [Core.ns, Packaging.ns, Util.ns],
            moduleDescription = Just "Error types for module and package validation"}
  where
    definitions = [
      conflictingModuleNamespaceError,
      conflictingVariantNameError,
      definitionNotInModuleNamespaceError,
      definitionsOutOfOrderError,
      duplicateDefinitionNameError,
      duplicateModuleNamespaceError,
      invalidDefinitionNameError,
      invalidModuleError,
      invalidNamespaceConventionError,
      invalidPackageError,
      invalidPackageNameError,
      missingDocumentationError]

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

definitionsOutOfOrderError :: Binding
definitionsOutOfOrderError = define "DefinitionsOutOfOrderError" $
  doc "Two consecutive definitions in a module's definitions list that are not in alphabetical order by local name. Hydra requires definitions to appear in lexicographic ASCII order of their local names." $
  T.record [
    "namespace">:
      doc "The namespace of the module containing the misordered definitions" $
      Packaging.namespace,
    "precedingName">:
      doc "The definition that appears first in the list" $
      Core.name,
    "followingName">:
      doc "The definition that appears next, but should sort earlier than precedingName" $
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

invalidDefinitionNameError :: Binding
invalidDefinitionNameError = define "InvalidDefinitionNameError" $
  doc "A definition whose local name does not match the expected naming convention. Term-level definitions must be camelCase; type-level definitions must be PascalCase." $
  T.record [
    "namespace">:
      doc "The namespace of the module containing the definition" $
      Packaging.namespace,
    "name">:
      doc "The definition name that violates the naming convention" $
      Core.name,
    "expectedConvention">:
      doc "The case convention the name should have followed (camel for term-level, pascal for type-level)" $
      Util.caseConvention]

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
    "definitionsOutOfOrder">:
      doc "Two consecutive definitions in the definitions list that are not in alphabetical order" $
      definitionsOutOfOrderError,
    "duplicateDefinitionName">:
      doc "Two or more definitions in the same module share the same name" $
      duplicateDefinitionNameError,
    "invalidDefinitionName">:
      doc "A definition whose local name does not match the expected naming convention" $
      invalidDefinitionNameError,
    "invalidNamespaceConvention">:
      doc "A module whose namespace does not match the namespace naming convention" $
      invalidNamespaceConventionError,
    "missingDocumentation">:
      doc "A top-level definition lacking a description annotation" $
      missingDocumentationError]

invalidNamespaceConventionError :: Binding
invalidNamespaceConventionError = define "InvalidNamespaceConventionError" $
  doc "A module whose namespace does not match the dotted-lowercase naming convention. Namespaces must be dot-separated lowercase segments, each starting with a letter, e.g. hydra.core or hydra.lib.lists." $
  T.record [
    "namespace">:
      doc "The namespace that violates the convention" $
      Packaging.namespace]

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
      invalidModuleError,
    "invalidPackageName">:
      doc "A package whose name does not match the package-name naming convention" $
      invalidPackageNameError]

invalidPackageNameError :: Binding
invalidPackageNameError = define "InvalidPackageNameError" $
  doc "A package whose name does not match the hyphen-separated lowercase naming convention. Package names must be hyphen-separated lowercase segments, each starting with a letter, e.g. hydra-kernel or hydra-python." $
  T.record [
    "packageName">:
      doc "The package name that violates the convention" $
      Packaging.packageName]

missingDocumentationError :: Binding
missingDocumentationError = define "MissingDocumentationError" $
  doc "A top-level definition whose term (or type, for type definitions) lacks a description annotation. Every definition in a Hydra module is expected to be wrapped in a doc \"...\" annotation at its top level." $
  T.record [
    "namespace">:
      doc "The namespace of the module containing the undocumented definition" $
      Packaging.namespace,
    "name">:
      doc "The name of the undocumented definition" $
      Core.name]
