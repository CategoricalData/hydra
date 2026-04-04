# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra namespaces, modules, and packages."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core
import hydra.graph

N = TypeVar("N")

class DefinitionTerm(Node["TermDefinition"]):
    r"""A term definition"""

class DefinitionType(Node["TypeDefinition"]):
    r"""A type definition"""

class _DefinitionMeta(type):
    def __getitem__(cls, item):
        return object

# A definition, which may be either a term or type definition.
class Definition(metaclass=_DefinitionMeta):
    r"""DefinitionTerm | DefinitionType"""

    TYPE_ = hydra.core.Name("hydra.packaging.Definition")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")

class FileExtension(Node[str]):
    r"""A file extension (without the dot), e.g. "json" or "py"."""

FileExtension.TYPE_ = hydra.core.Name("hydra.packaging.FileExtension")

@dataclass(frozen=True)
class Library:
    r"""A library of primitive functions."""

    namespace: Annotated[Namespace, "A common prefix for all primitive function names in the library"]
    prefix: Annotated[str, "A preferred namespace prefix for function names in the library"]
    primitives: Annotated[frozenlist[hydra.graph.Primitive], "The primitives defined in this library"]

    TYPE_ = hydra.core.Name("hydra.packaging.Library")
    NAMESPACE = hydra.core.Name("namespace")
    PREFIX = hydra.core.Name("prefix")
    PRIMITIVES = hydra.core.Name("primitives")

@dataclass(frozen=True)
class Module:
    r"""A logical collection of elements in the same namespace, having dependencies on zero or more other modules."""

    namespace: Annotated[Namespace, "A common prefix for all element names in the module"]
    definitions: Annotated[frozenlist[Definition], "The definitions in this module"]
    term_dependencies: Annotated[frozenlist[Namespace], "Any modules which the term expressions of this module directly depend upon"]
    type_dependencies: Annotated[frozenlist[Namespace], "Any modules which the type expressions of this module directly depend upon"]
    description: Annotated[Maybe[str], "An optional human-readable description of the module"]

    TYPE_ = hydra.core.Name("hydra.packaging.Module")
    NAMESPACE = hydra.core.Name("namespace")
    DEFINITIONS = hydra.core.Name("definitions")
    TERM_DEPENDENCIES = hydra.core.Name("termDependencies")
    TYPE_DEPENDENCIES = hydra.core.Name("typeDependencies")
    DESCRIPTION = hydra.core.Name("description")

class Namespace(Node[str]):
    r"""A prefix for element names."""

Namespace.TYPE_ = hydra.core.Name("hydra.packaging.Namespace")

@dataclass(frozen=True)
class Namespaces(Generic[N]):
    r"""A mapping from namespaces to values of type n, with a focus on one namespace."""

    focus: Annotated[tuple[Namespace, N], "The namespace in focus, together with its associated value"]
    mapping: Annotated[FrozenDict[Namespace, N], "A mapping of namespaces to values"]

    TYPE_ = hydra.core.Name("hydra.packaging.Namespaces")
    FOCUS = hydra.core.Name("focus")
    MAPPING = hydra.core.Name("mapping")

@dataclass(frozen=True)
class Package:
    r"""A package, which is a named collection of modules with metadata and dependencies."""

    name: Annotated[PackageName, "The name of the package"]
    modules: Annotated[frozenlist[Module], "The modules in this package"]
    dependencies: Annotated[frozenlist[PackageName], "The packages which this package depends on"]
    description: Annotated[Maybe[str], "An optional human-readable description of the package"]

    TYPE_ = hydra.core.Name("hydra.packaging.Package")
    NAME = hydra.core.Name("name")
    MODULES = hydra.core.Name("modules")
    DEPENDENCIES = hydra.core.Name("dependencies")
    DESCRIPTION = hydra.core.Name("description")

class PackageName(Node[str]):
    r"""The unique name of a package, e.g. "hydra-kernel" or "hydra-python"."""

PackageName.TYPE_ = hydra.core.Name("hydra.packaging.PackageName")

@dataclass(frozen=True)
class QualifiedName:
    r"""A qualified name consisting of an optional namespace together with a mandatory local name."""

    namespace: Annotated[Maybe[Namespace], "The optional namespace"]
    local: Annotated[str, "The local name"]

    TYPE_ = hydra.core.Name("hydra.packaging.QualifiedName")
    NAMESPACE = hydra.core.Name("namespace")
    LOCAL = hydra.core.Name("local")

@dataclass(frozen=True)
class TermDefinition:
    r"""A term-level definition, including a name, a term, and the type scheme of the term."""

    name: Annotated[hydra.core.Name, "The name of the term"]
    term: Annotated[hydra.core.Term, "The term being defined"]
    type: Annotated[Maybe[hydra.core.TypeScheme], "The type scheme of the term, including any class constraints"]

    TYPE_ = hydra.core.Name("hydra.packaging.TermDefinition")
    NAME = hydra.core.Name("name")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeDefinition:
    r"""A type-level definition, including a name and the type scheme."""

    name: Annotated[hydra.core.Name, "The name of the type"]
    type: Annotated[hydra.core.TypeScheme, "The type scheme being defined"]

    TYPE_ = hydra.core.Name("hydra.packaging.TypeDefinition")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
