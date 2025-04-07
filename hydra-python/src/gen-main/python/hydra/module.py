"""A model for Hydra namespaces and modules."""

from __future__ import annotations
from dataclasses import dataclass
from hydra.dsl.python import frozenlist, Node
from typing import Annotated
import hydra.core
import hydra.graph

class FileExtension(Node[str]):
    """A file extension (without the dot), e.g. "json" or "py"."""

FILE_EXTENSION__NAME = hydra.core.Name("hydra.module.FileExtension")

@dataclass
class Library:
    """A library of primitive functions."""
    
    namespace: Annotated[Namespace, "A common prefix for all primitive function names in the library"]
    prefix: Annotated[str, "A preferred namespace prefix for function names in the library"]
    primitives: Annotated[frozenlist[hydra.graph.Primitive], "The primitives defined in this library"]

LIBRARY__NAME = hydra.core.Name("hydra.module.Library")
LIBRARY__NAMESPACE__NAME = hydra.core.Name("namespace")
LIBRARY__PREFIX__NAME = hydra.core.Name("prefix")
LIBRARY__PRIMITIVES__NAME = hydra.core.Name("primitives")

@dataclass
class Module:
    """A logical collection of elements in the same namespace, having dependencies on zero or more other modules."""
    
    namespace: Annotated[Namespace, "A common prefix for all element names in the module"]
    elements: Annotated[frozenlist[hydra.graph.Element], "The elements defined in this module"]
    term_dependencies: Annotated[frozenlist[Module], "Any modules which the term expressions of this module directly depend upon"]
    type_dependencies: Annotated[frozenlist[Module], "Any modules which the type expressions of this module directly depend upon"]
    description: Annotated[str | None, "An optional human-readable description of the module"]

MODULE__NAME = hydra.core.Name("hydra.module.Module")
MODULE__NAMESPACE__NAME = hydra.core.Name("namespace")
MODULE__ELEMENTS__NAME = hydra.core.Name("elements")
MODULE__TERM_DEPENDENCIES__NAME = hydra.core.Name("termDependencies")
MODULE__TYPE_DEPENDENCIES__NAME = hydra.core.Name("typeDependencies")
MODULE__DESCRIPTION__NAME = hydra.core.Name("description")

class Namespace(Node[str]):
    """A prefix for element names."""

NAMESPACE__NAME = hydra.core.Name("hydra.module.Namespace")

@dataclass
class QualifiedName:
    """A qualified name consisting of an optional namespace together with a mandatory local name."""
    
    namespace: Namespace | None
    local: str

QUALIFIED_NAME__NAME = hydra.core.Name("hydra.module.QualifiedName")
QUALIFIED_NAME__NAMESPACE__NAME = hydra.core.Name("namespace")
QUALIFIED_NAME__LOCAL__NAME = hydra.core.Name("local")
