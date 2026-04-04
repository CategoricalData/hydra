package hydra.error.packaging

import hydra.core.*

import hydra.packaging.*

import hydra.core

import hydra.packaging

case class ConflictingModuleNamespaceError(first: hydra.packaging.Namespace, second: hydra.packaging.Namespace)

case class ConflictingVariantNameError(namespace: hydra.packaging.Namespace, typeName: hydra.core.Name,
   variantName: hydra.core.Name, conflictingName: hydra.core.Name)

case class DefinitionNotInModuleNamespaceError(namespace: hydra.packaging.Namespace, name: hydra.core.Name)

case class DuplicateDefinitionNameError(namespace: hydra.packaging.Namespace, name: hydra.core.Name)

case class DuplicateModuleNamespaceError(namespace: hydra.packaging.Namespace)

enum InvalidModuleError :
   case conflictingVariantName(value: hydra.error.packaging.ConflictingVariantNameError) extends InvalidModuleError
   case definitionNotInModuleNamespace(value: hydra.error.packaging.DefinitionNotInModuleNamespaceError) extends InvalidModuleError
   case duplicateDefinitionName(value: hydra.error.packaging.DuplicateDefinitionNameError) extends InvalidModuleError

enum InvalidPackageError :
   case conflictingModuleNamespace(value: hydra.error.packaging.ConflictingModuleNamespaceError) extends InvalidPackageError
   case duplicateModuleNamespace(value: hydra.error.packaging.DuplicateModuleNamespaceError) extends InvalidPackageError
   case invalidModule(value: hydra.error.packaging.InvalidModuleError) extends InvalidPackageError
