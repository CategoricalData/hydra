"""The extension to graphs of Hydra's core type system (hydra.gen.core)."""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from typing import Annotated, Generic, TypeVar

import hydra.gen.compute
import hydra.gen.core
from hydra.dsl.python import FrozenDict, frozenlist

A = TypeVar("A")

class Comparison(Enum):
    """An equality judgement: less than, equal to, or greater than."""
    
    LESS_THAN = "lessThan"
    
    EQUAL_TO = "equalTo"
    
    GREATER_THAN = "greaterThan"

COMPARISON__NAME = hydra.gen.core.Name("hydra.graph.Comparison")
COMPARISON__LESS_THAN__NAME = hydra.gen.core.Name("lessThan")
COMPARISON__EQUAL_TO__NAME = hydra.gen.core.Name("equalTo")
COMPARISON__GREATER_THAN__NAME = hydra.gen.core.Name("greaterThan")

@dataclass
class Element:
    """A graph element, having a name, data term (value), and schema term (type)."""
    
    name: hydra.gen.core.Name
    term: hydra.gen.core.Term
    type: hydra.gen.core.TypeScheme | None

ELEMENT__NAME = hydra.gen.core.Name("hydra.graph.Element")
ELEMENT__NAME__NAME = hydra.gen.core.Name("name")
ELEMENT__TERM__NAME = hydra.gen.core.Name("term")
ELEMENT__TYPE__NAME = hydra.gen.core.Name("type")

@dataclass
class Graph:
    """A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph."""
    
    elements: Annotated[FrozenDict[hydra.gen.core.Name, Element], "All of the elements in the graph"]
    environment: Annotated[FrozenDict[hydra.gen.core.Name, hydra.gen.core.Term | None], "The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)"]
    types: Annotated[FrozenDict[hydra.gen.core.Name, hydra.gen.core.TypeScheme], "The typing environment of the graph"]
    body: Annotated[hydra.gen.core.Term, "The body of the term which generated this context"]
    primitives: Annotated[FrozenDict[hydra.gen.core.Name, Primitive], "All supported primitive constants and functions, by name"]
    schema: Annotated[Graph | None, "The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph."]

GRAPH__NAME = hydra.gen.core.Name("hydra.graph.Graph")
GRAPH__ELEMENTS__NAME = hydra.gen.core.Name("elements")
GRAPH__ENVIRONMENT__NAME = hydra.gen.core.Name("environment")
GRAPH__TYPES__NAME = hydra.gen.core.Name("types")
GRAPH__BODY__NAME = hydra.gen.core.Name("body")
GRAPH__PRIMITIVES__NAME = hydra.gen.core.Name("primitives")
GRAPH__SCHEMA__NAME = hydra.gen.core.Name("schema")

@dataclass
class Primitive:
    """A built-in function."""
    
    name: Annotated[hydra.gen.core.Name, "The unique name of the primitive function"]
    type: Annotated[hydra.gen.core.TypeScheme, "The type signature of the primitive function"]
    implementation: Annotated[Callable[[frozenlist[hydra.gen.core.Term]], hydra.gen.compute.Flow[Graph, hydra.gen.core.Term]], "A concrete implementation of the primitive function"]

PRIMITIVE__NAME = hydra.gen.core.Name("hydra.graph.Primitive")
PRIMITIVE__NAME__NAME = hydra.gen.core.Name("name")
PRIMITIVE__TYPE__NAME = hydra.gen.core.Name("type")
PRIMITIVE__IMPLEMENTATION__NAME = hydra.gen.core.Name("implementation")

@dataclass
class TermCoder(Generic[A]):
    """A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."""
    
    type: hydra.gen.core.Type
    coder: hydra.gen.compute.Coder[Graph, Graph, hydra.gen.core.Term, A]

TERM_CODER__NAME = hydra.gen.core.Name("hydra.graph.TermCoder")
TERM_CODER__TYPE__NAME = hydra.gen.core.Name("type")
TERM_CODER__CODER__NAME = hydra.gen.core.Name("coder")

class TypeClass(Enum):
    """Any of a small number of built-in type classes."""
    
    EQUALITY = "equality"
    
    ORDERING = "ordering"

TYPE_CLASS__NAME = hydra.gen.core.Name("hydra.graph.TypeClass")
TYPE_CLASS__EQUALITY__NAME = hydra.gen.core.Name("equality")
TYPE_CLASS__ORDERING__NAME = hydra.gen.core.Name("ordering")
