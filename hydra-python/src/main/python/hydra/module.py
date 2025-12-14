# Note: this is an automatically generated file. Do not edit.

r"""A model for Hydra namespaces and modules."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core
import hydra.graph

N = TypeVar("N")

class DefinitionTerm(Node["TermDefinition"]):
    r"""A term definition."""

class DefinitionType(Node["TypeDefinition"]):
    r"""A type definition."""

# A definition, which may be either a term or type definition.
Definition: TypeAlias = "DefinitionTerm | DefinitionType"

DEFINITION__NAME = hydra.core.Name("hydra.module.Definition")
DEFINITION__TERM__NAME = hydra.core.Name("term")
DEFINITION__TYPE__NAME = hydra.core.Name("type")

class FileExtension(Node[str]):
    r"""A file extension (without the dot), e.g. "json" or "py"."""

FILE_EXTENSION__NAME = hydra.core.Name("hydra.module.FileExtension")

@dataclass(frozen=True)
class Library:
    r"""A library of primitive functions."""
    
    namespace: Annotated[Namespace, "A common prefix for all primitive function names in the library"]
    prefix: Annotated[str, "A preferred namespace prefix for function names in the library"]
    primitives: Annotated[frozenlist[hydra.graph.Primitive], "The primitives defined in this library"]

LIBRARY__NAME = hydra.core.Name("hydra.module.Library")
LIBRARY__NAMESPACE__NAME = hydra.core.Name("namespace")
LIBRARY__PREFIX__NAME = hydra.core.Name("prefix")
LIBRARY__PRIMITIVES__NAME = hydra.core.Name("primitives")

@dataclass(frozen=True)
class Module:
    r"""A logical collection of elements in the same namespace, having dependencies on zero or more other modules."""
    
    namespace: Annotated[Namespace, "A common prefix for all element names in the module"]
    elements: Annotated[frozenlist[hydra.core.Binding], "The elements defined in this module"]
    term_dependencies: Annotated[frozenlist[Module], "Any modules which the term expressions of this module directly depend upon"]
    type_dependencies: Annotated[frozenlist[Module], "Any modules which the type expressions of this module directly depend upon"]
    description: Annotated[Maybe[str], "An optional human-readable description of the module"]

MODULE__NAME = hydra.core.Name("hydra.module.Module")
MODULE__NAMESPACE__NAME = hydra.core.Name("namespace")
MODULE__ELEMENTS__NAME = hydra.core.Name("elements")
MODULE__TERM_DEPENDENCIES__NAME = hydra.core.Name("termDependencies")
MODULE__TYPE_DEPENDENCIES__NAME = hydra.core.Name("typeDependencies")
MODULE__DESCRIPTION__NAME = hydra.core.Name("description")

class Namespace(Node[str]):
    r"""A prefix for element names."""

NAMESPACE__NAME = hydra.core.Name("hydra.module.Namespace")

@dataclass(frozen=True)
class Namespaces(Generic[N]):
    r"""A mapping from namespaces to values of type n, with a focus on one namespace."""
    
    focus: Annotated[tuple[Namespace, N], "The namespace in focus, together with its associated value"]
    mapping: Annotated[FrozenDict[Namespace, N], "A mapping of namespaces to values"]

NAMESPACES__NAME = hydra.core.Name("hydra.module.Namespaces")
NAMESPACES__FOCUS__NAME = hydra.core.Name("focus")
NAMESPACES__MAPPING__NAME = hydra.core.Name("mapping")

@dataclass(frozen=True)
class QualifiedName:
    r"""A qualified name consisting of an optional namespace together with a mandatory local name."""
    
    namespace: Annotated[Maybe[Namespace], "The optional namespace"]
    local: Annotated[str, "The local name"]

QUALIFIED_NAME__NAME = hydra.core.Name("hydra.module.QualifiedName")
QUALIFIED_NAME__NAMESPACE__NAME = hydra.core.Name("namespace")
QUALIFIED_NAME__LOCAL__NAME = hydra.core.Name("local")

@dataclass(frozen=True)
class TermDefinition:
    r"""A term-level definition, including a name, a term, and the type of the term."""
    
    name: Annotated[hydra.core.Name, "The name of the term"]
    term: Annotated[hydra.core.Term, "The term being defined"]
    type: Annotated[hydra.core.Type, "The type of the term"]

TERM_DEFINITION__NAME = hydra.core.Name("hydra.module.TermDefinition")
TERM_DEFINITION__NAME__NAME = hydra.core.Name("name")
TERM_DEFINITION__TERM__NAME = hydra.core.Name("term")
TERM_DEFINITION__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeDefinition:
    r"""A type-level definition, including a name and the type."""
    
    name: Annotated[hydra.core.Name, "The name of the type"]
    type: Annotated[hydra.core.Type, "The type being defined"]

TYPE_DEFINITION__NAME = hydra.core.Name("hydra.module.TypeDefinition")
TYPE_DEFINITION__NAME__NAME = hydra.core.Name("name")
TYPE_DEFINITION__TYPE__NAME = hydra.core.Name("type")
