# Note: this is an automatically generated file. Do not edit.

r"""Error types for module and package validation."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.packaging

@dataclass(frozen=True)
class ConflictingModuleNamespaceError:
    r"""A module namespace which, when mapped to a target language's directory or package structure, conflicts with another module's mapped namespace. For example, hydra.foo.bar and hydra.fooBar might both map to the same directory in a case-insensitive filesystem."""

    first: Annotated[hydra.packaging.Namespace, "The first module namespace"]
    second: Annotated[hydra.packaging.Namespace, "The second module namespace that conflicts with the first"]

    TYPE_ = hydra.core.Name("hydra.error.packaging.ConflictingModuleNamespaceError")
    FIRST = hydra.core.Name("first")
    SECOND = hydra.core.Name("second")

@dataclass(frozen=True)
class ConflictingVariantNameError:
    r"""A union type variant name which, when capitalized and concatenated with its type name, conflicts with another type definition name. For example, a union type Foo with a variant bar produces FooBar, which conflicts with an existing type definition FooBar. This is currently a problem only for the Haskell target."""

    namespace: Annotated[hydra.packaging.Namespace, "The namespace of the module containing the conflict"]
    type_name: Annotated[hydra.core.Name, "The name of the union type"]
    variant_name: Annotated[hydra.core.Name, "The name of the variant field causing the conflict"]
    conflicting_name: Annotated[hydra.core.Name, "The name of the other type definition that conflicts with the generated constructor name"]

    TYPE_ = hydra.core.Name("hydra.error.packaging.ConflictingVariantNameError")
    NAMESPACE = hydra.core.Name("namespace")
    TYPE_NAME = hydra.core.Name("typeName")
    VARIANT_NAME = hydra.core.Name("variantName")
    CONFLICTING_NAME = hydra.core.Name("conflictingName")

@dataclass(frozen=True)
class DefinitionNotInModuleNamespaceError:
    r"""A definition whose name does not have the module's namespace as a prefix. If the module namespace is foo.bar, all definition names must have the form foo.bar.quux."""

    namespace: Annotated[hydra.packaging.Namespace, "The namespace of the module"]
    name: Annotated[hydra.core.Name, "The definition name that does not match the module namespace"]

    TYPE_ = hydra.core.Name("hydra.error.packaging.DefinitionNotInModuleNamespaceError")
    NAMESPACE = hydra.core.Name("namespace")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class DuplicateDefinitionNameError:
    r"""Two or more definitions in the same module share the same name."""

    namespace: Annotated[hydra.packaging.Namespace, "The namespace of the module containing the duplicates"]
    name: Annotated[hydra.core.Name, "The duplicated definition name"]

    TYPE_ = hydra.core.Name("hydra.error.packaging.DuplicateDefinitionNameError")
    NAMESPACE = hydra.core.Name("namespace")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class DuplicateModuleNamespaceError:
    r"""Two or more modules in the same package share the same namespace."""

    namespace: Annotated[hydra.packaging.Namespace, "The duplicated module namespace"]

    TYPE_ = hydra.core.Name("hydra.error.packaging.DuplicateModuleNamespaceError")
    NAMESPACE = hydra.core.Name("namespace")

class InvalidModuleErrorConflictingVariantName(Node["ConflictingVariantNameError"]):
    r"""A union variant name that conflicts with another type definition when mapped to a target language"""

class InvalidModuleErrorDefinitionNotInModuleNamespace(Node["DefinitionNotInModuleNamespaceError"]):
    r"""A definition whose name does not have the module's namespace as a prefix"""

class InvalidModuleErrorDuplicateDefinitionName(Node["DuplicateDefinitionNameError"]):
    r"""Two or more definitions in the same module share the same name"""

class _InvalidModuleErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a module is invalid.
class InvalidModuleError(metaclass=_InvalidModuleErrorMeta):
    r"""InvalidModuleErrorConflictingVariantName | InvalidModuleErrorDefinitionNotInModuleNamespace | InvalidModuleErrorDuplicateDefinitionName"""

    TYPE_ = hydra.core.Name("hydra.error.packaging.InvalidModuleError")
    CONFLICTING_VARIANT_NAME = hydra.core.Name("conflictingVariantName")
    DEFINITION_NOT_IN_MODULE_NAMESPACE = hydra.core.Name("definitionNotInModuleNamespace")
    DUPLICATE_DEFINITION_NAME = hydra.core.Name("duplicateDefinitionName")

class InvalidPackageErrorConflictingModuleNamespace(Node["ConflictingModuleNamespaceError"]):
    r"""Two module namespaces that conflict when mapped to a target language"""

class InvalidPackageErrorDuplicateModuleNamespace(Node["DuplicateModuleNamespaceError"]):
    r"""Two or more modules in the same package share the same namespace"""

class InvalidPackageErrorInvalidModule(Node["InvalidModuleError"]):
    r"""A module within the package is invalid"""

class _InvalidPackageErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a package is invalid.
class InvalidPackageError(metaclass=_InvalidPackageErrorMeta):
    r"""InvalidPackageErrorConflictingModuleNamespace | InvalidPackageErrorDuplicateModuleNamespace | InvalidPackageErrorInvalidModule"""

    TYPE_ = hydra.core.Name("hydra.error.packaging.InvalidPackageError")
    CONFLICTING_MODULE_NAMESPACE = hydra.core.Name("conflictingModuleNamespace")
    DUPLICATE_MODULE_NAMESPACE = hydra.core.Name("duplicateModuleNamespace")
    INVALID_MODULE = hydra.core.Name("invalidModule")
