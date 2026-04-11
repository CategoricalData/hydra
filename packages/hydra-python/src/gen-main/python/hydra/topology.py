# Note: this is an automatically generated file. Do not edit.

r"""A model for simple graphs as adjacency lists."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core

A = TypeVar("A")

# A directed graph represented as an adjacency list mapping vertices to their outgoing neighbors.
Graph: TypeAlias = "FrozenDict[Vertex, frozenlist[Vertex]]"

@dataclass(frozen=True)
class OrderingIsomorphism(Generic[A]):
    encode: Annotated[Callable[[frozenlist[A]], frozenlist[A]], "Mapping from source ordering to target ordering"]
    decode: Annotated[Callable[[frozenlist[A]], frozenlist[A]], "Mapping from target ordering to source ordering"]

    TYPE_ = hydra.core.Name("hydra.topology.OrderingIsomorphism")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

@dataclass(frozen=True)
class TarjanState:
    counter: Annotated[int, "Next available index for vertices in the DFS traversal"]
    indices: Annotated[FrozenDict[Vertex, int], "Mapping from vertices to their indices in the DFS traversal"]
    low_links: Annotated[FrozenDict[Vertex, int], "Mapping from vertices to their lowest reachable index in the DFS traversal"]
    stack: Annotated[frozenlist[Vertex], "Current DFS stack, with vertices in reverse order"]
    on_stack: Annotated[frozenset[Vertex], "Set of vertices currently on the stack, for quick lookup"]
    sccs: Annotated[frozenlist[frozenlist[Vertex]], "Accumulated strongly connected components, each a list of vertices"]

    TYPE_ = hydra.core.Name("hydra.topology.TarjanState")
    COUNTER = hydra.core.Name("counter")
    INDICES = hydra.core.Name("indices")
    LOW_LINKS = hydra.core.Name("lowLinks")
    STACK = hydra.core.Name("stack")
    ON_STACK = hydra.core.Name("onStack")
    SCCS = hydra.core.Name("sccs")

# A graph vertex, represented as a 32-bit integer identifier.
Vertex: TypeAlias = "int"
