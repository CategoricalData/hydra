# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error.packaging."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

def conflicting_module_namespace_error(first: hydra.phantoms.TTerm[hydra.packaging.Namespace], second: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("first"), first.value), hydra.core.Field(hydra.core.Name("second"), second.value))))))

def conflicting_module_namespace_error_first(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), hydra.core.Name("first")))), x.value))))

def conflicting_module_namespace_error_second(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), hydra.core.Name("second")))), x.value))))

def conflicting_module_namespace_error_with_first(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingModuleNamespaceError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("first"), new_val.value), hydra.core.Field(hydra.core.Name("second"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), hydra.core.Name("second")))), original.value)))))))))

def conflicting_module_namespace_error_with_second(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingModuleNamespaceError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("first"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError"), hydra.core.Name("first")))), original.value)))), hydra.core.Field(hydra.core.Name("second"), new_val.value))))))

def conflicting_variant_name_error(namespace: hydra.phantoms.TTerm[hydra.packaging.Namespace], type_name: hydra.phantoms.TTerm[hydra.core.Name], variant_name: hydra.phantoms.TTerm[hydra.core.Name], conflicting_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), (hydra.core.Field(hydra.core.Name("namespace"), namespace.value), hydra.core.Field(hydra.core.Name("typeName"), type_name.value), hydra.core.Field(hydra.core.Name("variantName"), variant_name.value), hydra.core.Field(hydra.core.Name("conflictingName"), conflicting_name.value))))))

def conflicting_variant_name_error_conflicting_name(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("conflictingName")))), x.value))))

def conflicting_variant_name_error_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("namespace")))), x.value))))

def conflicting_variant_name_error_type_name(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("typeName")))), x.value))))

def conflicting_variant_name_error_variant_name(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("variantName")))), x.value))))

def conflicting_variant_name_error_with_conflicting_name(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("namespace")))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("variantName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("variantName")))), original.value)))), hydra.core.Field(hydra.core.Name("conflictingName"), new_val.value))))))

def conflicting_variant_name_error_with_namespace(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), (hydra.core.Field(hydra.core.Name("namespace"), new_val.value), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("variantName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("variantName")))), original.value)))), hydra.core.Field(hydra.core.Name("conflictingName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("conflictingName")))), original.value)))))))))

def conflicting_variant_name_error_with_type_name(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("namespace")))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), new_val.value), hydra.core.Field(hydra.core.Name("variantName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("variantName")))), original.value)))), hydra.core.Field(hydra.core.Name("conflictingName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("conflictingName")))), original.value)))))))))

def conflicting_variant_name_error_with_variant_name(original: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("namespace")))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("typeName")))), original.value)))), hydra.core.Field(hydra.core.Name("variantName"), new_val.value), hydra.core.Field(hydra.core.Name("conflictingName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError"), hydra.core.Name("conflictingName")))), original.value)))))))))

def definition_not_in_module_namespace_error(namespace: hydra.phantoms.TTerm[hydra.packaging.Namespace], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("namespace"), namespace.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def definition_not_in_module_namespace_error_name(x: hydra.phantoms.TTerm[hydra.error.packaging.DefinitionNotInModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), hydra.core.Name("name")))), x.value))))

def definition_not_in_module_namespace_error_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.DefinitionNotInModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), hydra.core.Name("namespace")))), x.value))))

def definition_not_in_module_namespace_error_with_name(original: hydra.phantoms.TTerm[hydra.error.packaging.DefinitionNotInModuleNamespaceError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), hydra.core.Name("namespace")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def definition_not_in_module_namespace_error_with_namespace(original: hydra.phantoms.TTerm[hydra.error.packaging.DefinitionNotInModuleNamespaceError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("namespace"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError"), hydra.core.Name("name")))), original.value)))))))))

def duplicate_definition_name_error(namespace: hydra.phantoms.TTerm[hydra.packaging.Namespace], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), (hydra.core.Field(hydra.core.Name("namespace"), namespace.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_definition_name_error_name(x: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateDefinitionNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), hydra.core.Name("name")))), x.value))))

def duplicate_definition_name_error_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateDefinitionNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), hydra.core.Name("namespace")))), x.value))))

def duplicate_definition_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateDefinitionNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), (hydra.core.Field(hydra.core.Name("namespace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), hydra.core.Name("namespace")))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def duplicate_definition_name_error_with_namespace(original: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateDefinitionNameError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), (hydra.core.Field(hydra.core.Name("namespace"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError"), hydra.core.Name("name")))), original.value)))))))))

def duplicate_module_namespace_error(namespace: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("namespace"), namespace.value),)))))

def duplicate_module_namespace_error_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermProject(hydra.core.Projection(hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), hydra.core.Name("namespace")))), x.value))))

def duplicate_module_namespace_error_with_namespace(original: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateModuleNamespaceError], new_val: hydra.phantoms.TTerm[hydra.packaging.Namespace]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError"), (hydra.core.Field(hydra.core.Name("namespace"), new_val.value),)))))

def invalid_module_error_conflicting_variant_name(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingVariantNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidModuleError"), hydra.core.Field(hydra.core.Name("conflictingVariantName"), x.value)))))

def invalid_module_error_definition_not_in_module_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.DefinitionNotInModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidModuleError"), hydra.core.Field(hydra.core.Name("definitionNotInModuleNamespace"), x.value)))))

def invalid_module_error_duplicate_definition_name(x: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateDefinitionNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidModuleError"), hydra.core.Field(hydra.core.Name("duplicateDefinitionName"), x.value)))))

def invalid_package_error_conflicting_module_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.ConflictingModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidPackageError"), hydra.core.Field(hydra.core.Name("conflictingModuleNamespace"), x.value)))))

def invalid_package_error_duplicate_module_namespace(x: hydra.phantoms.TTerm[hydra.error.packaging.DuplicateModuleNamespaceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidPackageError"), hydra.core.Field(hydra.core.Name("duplicateModuleNamespace"), x.value)))))

def invalid_package_error_invalid_module(x: hydra.phantoms.TTerm[hydra.error.packaging.InvalidModuleError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.packaging.InvalidPackageError"), hydra.core.Field(hydra.core.Name("invalidModule"), x.value)))))
