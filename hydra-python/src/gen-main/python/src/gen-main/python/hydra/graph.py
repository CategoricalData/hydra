# Note: this is an automatically generated file. Do not edit.

r"""The extension to graphs of Hydra's core type system (hydra.core)."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.compute
import hydra.core

A = TypeVar("A")

@dataclass(frozen=True)
class Graph:
    r"""A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph."""
    
    elements: Annotated[frozenlist[hydra.core.Binding], "All of the elements in the graph"]
    environment: Annotated[FrozenDict[hydra.core.Name, Maybe[hydra.core.Term]], "The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)"]
    types: Annotated[FrozenDict[hydra.core.Name, hydra.core.TypeScheme], "The typing environment of the graph"]
    body: Annotated[hydra.core.Term, "The body of the term which generated this context"]
    primitives: Annotated[FrozenDict[hydra.core.Name, Primitive], "All supported primitive constants and functions, by name"]
    schema: Annotated[Maybe[Graph], "The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph."]
    
    TYPE_ = hydra.core.Name("hydra.graph.Graph")
    ELEMENTS = hydra.core.Name("elements")
    ENVIRONMENT = hydra.core.Name("environment")
    TYPES = hydra.core.Name("types")
    BODY = hydra.core.Name("body")
    PRIMITIVES = hydra.core.Name("primitives")
    SCHEMA = hydra.core.Name("schema")

@dataclass(frozen=True)
class Primitive:
    r"""A built-in function."""
    
    name: Annotated[hydra.core.Name, "The unique name of the primitive function"]
    type: Annotated[hydra.core.TypeScheme, "The type signature of the primitive function"]
    implementation: Annotated[Callable[[frozenlist[hydra.core.Term]], hydra.compute.Flow[Graph, hydra.core.Term]], "A concrete implementation of the primitive function"]
    
    TYPE_ = hydra.core.Name("hydra.graph.Primitive")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    IMPLEMENTATION = hydra.core.Name("implementation")

@dataclass(frozen=True)
class TermCoder(Generic[A]):
    r"""A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms."""
    
    type: Annotated[hydra.core.Type, "The Hydra type of encoded terms"]
    coder: Annotated[hydra.compute.Coder[Graph, Graph, hydra.core.Term, A], "A coder between Hydra terms and instances of the given type"]
    
    TYPE_ = hydra.core.Name("hydra.graph.TermCoder")
    TYPE = hydra.core.Name("type")
    CODER = hydra.core.Name("coder")
