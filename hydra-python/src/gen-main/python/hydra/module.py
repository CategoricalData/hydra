# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra namespaces and modules."""

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

    TYPE_ = hydra.core.Name("hydra.module.Definition")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")

class FileExtension(Node[str]):
    r"""A file extension (without the dot), e.g. "json" or "py"."""

FileExtension.TYPE_ = hydra.core.Name("hydra.module.FileExtension")

@dataclass(frozen=True)
class Library:
    r"""A library of primitive functions."""

    namespace: Annotated[Namespace, "A common prefix for all primitive function names in the library"]
    prefix: Annotated[str, "A preferred namespace prefix for function names in the library"]
    primitives: Annotated[frozenlist[hydra.graph.Primitive], "The primitives defined in this library"]

    TYPE_ = hydra.core.Name("hydra.module.Library")
    NAMESPACE = hydra.core.Name("namespace")
    PREFIX = hydra.core.Name("prefix")
    PRIMITIVES = hydra.core.Name("primitives")

@dataclass(frozen=True)
class Module:
    r"""A logical collection of elements in the same namespace, having dependencies on zero or more other modules."""

    namespace: Annotated[Namespace, "A common prefix for all element names in the module"]
    elements: Annotated[frozenlist[hydra.core.Binding], "The elements defined in this module"]
    term_dependencies: Annotated[frozenlist[Namespace], "Any modules which the term expressions of this module directly depend upon"]
    type_dependencies: Annotated[frozenlist[Namespace], "Any modules which the type expressions of this module directly depend upon"]
    description: Annotated[Maybe[str], "An optional human-readable description of the module"]

    TYPE_ = hydra.core.Name("hydra.module.Module")
    NAMESPACE = hydra.core.Name("namespace")
    ELEMENTS = hydra.core.Name("elements")
    TERM_DEPENDENCIES = hydra.core.Name("termDependencies")
    TYPE_DEPENDENCIES = hydra.core.Name("typeDependencies")
    DESCRIPTION = hydra.core.Name("description")

class Namespace(Node[str]):
    r"""A prefix for element names."""

Namespace.TYPE_ = hydra.core.Name("hydra.module.Namespace")

@dataclass(frozen=True)
class Namespaces(Generic[N]):
    r"""A mapping from namespaces to values of type n, with a focus on one namespace."""

    focus: Annotated[tuple[Namespace, N], "The namespace in focus, together with its associated value"]
    mapping: Annotated[FrozenDict[Namespace, N], "A mapping of namespaces to values"]

    TYPE_ = hydra.core.Name("hydra.module.Namespaces")
    FOCUS = hydra.core.Name("focus")
    MAPPING = hydra.core.Name("mapping")

@dataclass(frozen=True)
class QualifiedName:
    r"""A qualified name consisting of an optional namespace together with a mandatory local name."""

    namespace: Annotated[Maybe[Namespace], "The optional namespace"]
    local: Annotated[str, "The local name"]

    TYPE_ = hydra.core.Name("hydra.module.QualifiedName")
    NAMESPACE = hydra.core.Name("namespace")
    LOCAL = hydra.core.Name("local")

@dataclass(frozen=True)
class TermDefinition:
    r"""A term-level definition, including a name, a term, and the type scheme of the term."""

    name: Annotated[hydra.core.Name, "The name of the term"]
    term: Annotated[hydra.core.Term, "The term being defined"]
    type: Annotated[hydra.core.TypeScheme, "The type scheme of the term, including any class constraints"]

    TYPE_ = hydra.core.Name("hydra.module.TermDefinition")
    NAME = hydra.core.Name("name")
    TERM = hydra.core.Name("term")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeDefinition:
    r"""A type-level definition, including a name and the type."""

    name: Annotated[hydra.core.Name, "The name of the type"]
    type: Annotated[hydra.core.Type, "The type being defined"]

    TYPE_ = hydra.core.Name("hydra.module.TypeDefinition")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
