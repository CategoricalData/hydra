# Note: this is an automatically generated file. Do not edit.

r"""A model for simple graphs as adjacency lists."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, Generic, TypeVar
import hydra.core

A = TypeVar("A")

# A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors.
type Graph = FrozenDict[Vertex, frozenlist[Vertex]]

GRAPH__NAME = hydra.core.Name("hydra.topology.Graph")

@dataclass
class OrderingIsomorphism(Generic[A]):
    encode: Annotated[Callable[[frozenlist[A]], frozenlist[A]], "Mapping from source ordering to target ordering"]
    decode: Annotated[Callable[[frozenlist[A]], frozenlist[A]], "Mapping from target ordering to source ordering"]

ORDERING_ISOMORPHISM__NAME = hydra.core.Name("hydra.topology.OrderingIsomorphism")
ORDERING_ISOMORPHISM__ENCODE__NAME = hydra.core.Name("encode")
ORDERING_ISOMORPHISM__DECODE__NAME = hydra.core.Name("decode")

@dataclass
class TarjanState:
    counter: Annotated[int, "Next available index for vertices in the DFS traversal"]
    indices: Annotated[FrozenDict[Vertex, int], "Mapping from vertices to their indices in the DFS traversal"]
    low_links: Annotated[FrozenDict[Vertex, int], "Mapping from vertices to their lowest reachable index in the DFS traversal"]
    stack: Annotated[frozenlist[Vertex], "Current DFS stack, with vertices in reverse order"]
    on_stack: Annotated[frozenset[Vertex], "Set of vertices currently on the stack, for quick lookup"]
    sccs: Annotated[frozenlist[frozenlist[Vertex]], "Accumulated strongly connected components, each a list of vertices"]

TARJAN_STATE__NAME = hydra.core.Name("hydra.topology.TarjanState")
TARJAN_STATE__COUNTER__NAME = hydra.core.Name("counter")
TARJAN_STATE__INDICES__NAME = hydra.core.Name("indices")
TARJAN_STATE__LOW_LINKS__NAME = hydra.core.Name("lowLinks")
TARJAN_STATE__STACK__NAME = hydra.core.Name("stack")
TARJAN_STATE__ON_STACK__NAME = hydra.core.Name("onStack")
TARJAN_STATE__SCCS__NAME = hydra.core.Name("sccs")

# A graph vertex, represented as a 32-bit integer identifier.
type Vertex = int

VERTEX__NAME = hydra.core.Name("hydra.topology.Vertex")
