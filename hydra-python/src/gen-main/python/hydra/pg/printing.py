# Note: this is an automatically generated file. Do not edit.

r"""Printing functions for property graph elements."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.pairs
import hydra.lib.strings
import hydra.pg.model

T0 = TypeVar("T0")

def print_property(print_value: Callable[[T0], str], key: hydra.pg.model.PropertyKey, value: T0) -> str:
    r"""Print a property using the provided value printer."""

    return hydra.lib.strings.cat((key.value, ": ", print_value(value)))

def print_edge(print_value: Callable[[T0], str], edge: hydra.pg.model.Edge[T0]) -> str:
    r"""Print an edge using the provided value printer."""

    @lru_cache(1)
    def label() -> str:
        return edge.label.value
    @lru_cache(1)
    def id() -> str:
        return print_value(edge.id)
    @lru_cache(1)
    def out_id() -> str:
        return print_value(edge.out)
    @lru_cache(1)
    def in_id() -> str:
        return print_value(edge.in_)
    @lru_cache(1)
    def props() -> str:
        return hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda p: print_property(print_value, hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.maps.to_list(edge.properties)))
    return hydra.lib.strings.cat((id(), ": ", "(", out_id(), ")-[:", label(), " {", props(), "}]->(", in_id(), ")"))

def print_vertex(print_value: Callable[[T0], str], vertex: hydra.pg.model.Vertex[T0]) -> str:
    r"""Print a vertex using the provided value printer."""

    @lru_cache(1)
    def label() -> str:
        return vertex.label.value
    @lru_cache(1)
    def id() -> str:
        return print_value(vertex.id)
    @lru_cache(1)
    def props() -> str:
        return hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda p: print_property(print_value, hydra.lib.pairs.first(p), hydra.lib.pairs.second(p))), hydra.lib.maps.to_list(vertex.properties)))
    return hydra.lib.strings.cat((id(), ": (", label(), ": {", props(), "})"))

def print_lazy_graph(print_value: Callable[[T0], str], lg: hydra.pg.model.LazyGraph[T0]) -> str:
    r"""Print a lazy graph using the provided value printer."""

    @lru_cache(1)
    def vertices() -> frozenlist[hydra.pg.model.Vertex[T0]]:
        return lg.vertices
    @lru_cache(1)
    def edges() -> frozenlist[hydra.pg.model.Edge[T0]]:
        return lg.edges
    return hydra.lib.strings.cat(("vertices:", hydra.lib.strings.cat(hydra.lib.lists.map((lambda v: hydra.lib.strings.cat(("\n\t", print_vertex(print_value, v)))), vertices())), "\nedges:", hydra.lib.strings.cat(hydra.lib.lists.map((lambda e: hydra.lib.strings.cat(("\n\t", print_edge(print_value, e)))), edges()))))

def print_graph(print_value: Callable[[T0], str], graph: hydra.pg.model.Graph[T0]) -> str:
    r"""Print a graph using the provided value printer."""

    return print_lazy_graph(print_value, hydra.pg.model.LazyGraph(hydra.lib.maps.elems(graph.vertices), hydra.lib.maps.elems(graph.edges)))
