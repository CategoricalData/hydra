# Note: this is an automatically generated file. Do not edit.

r"""Functions for constructing GraphSON vertices from property graph vertices."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Just, Left, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.coders
import hydra.context
import hydra.core
import hydra.errors
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.pg.graphson.coder
import hydra.pg.graphson.syntax
import hydra.pg.model

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def edge_property_to_graphson(encode_value: Callable[[T0], Either[T1, T2]], prop: tuple[hydra.pg.model.PropertyKey, T0]) -> Either[T1, tuple[hydra.pg.graphson.syntax.PropertyKey, T1]]:
    r"""Convert a property graph edge property to a GraphSON property."""

    return hydra.lib.eithers.map((lambda gv: (hydra.pg.graphson.syntax.PropertyKey(hydra.lib.pairs.first(prop).value), gv)), encode_value(hydra.lib.pairs.second(prop)))

def adjacent_edge_to_graphson(encode_value: Callable[[T0], Either[T1, hydra.pg.graphson.syntax.Value]], edge: hydra.pg.model.AdjacentEdge[T0]) -> Either[hydra.pg.graphson.syntax.Value, tuple[hydra.pg.graphson.syntax.EdgeLabel, hydra.pg.graphson.syntax.AdjacentEdge]]:
    r"""Convert a property graph adjacent edge to a GraphSON adjacent edge."""

    @lru_cache(1)
    def label() -> hydra.pg.model.EdgeLabel:
        return edge.label
    @lru_cache(1)
    def edge_id() -> T0:
        return edge.id
    @lru_cache(1)
    def vertex_id() -> T0:
        return edge.vertex
    @lru_cache(1)
    def props() -> FrozenDict[hydra.pg.model.PropertyKey, T0]:
        return edge.properties
    return hydra.lib.eithers.bind(encode_value(edge_id()), (lambda gid: hydra.lib.eithers.bind(encode_value(vertex_id()), (lambda gv: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: edge_property_to_graphson(encode_value, v1)), hydra.lib.maps.to_list(props())), (lambda prop_pairs: Right((hydra.pg.graphson.syntax.EdgeLabel(label().value), hydra.pg.graphson.syntax.AdjacentEdge(gid, gv, hydra.lib.maps.from_list(prop_pairs))))))))))

def aggregate_map(pairs: frozenlist[tuple[T0, T1]]) -> FrozenDict[T0, frozenlist[tuple[T0, T1]]]:
    r"""Aggregate a list of key-value pairs into a map where each key maps to a list of values."""

    return hydra.lib.lists.foldl((lambda m, p: (k := hydra.lib.pairs.first(p), v := hydra.lib.pairs.second(p), existing := hydra.lib.maps.lookup(k, m), hydra.lib.maps.insert(k, hydra.lib.maybes.maybe((lambda : hydra.lib.lists.pure(v)), (lambda vs: hydra.lib.lists.cons(v, vs)), existing), m))[3]), hydra.lib.maps.empty(), pairs)

@lru_cache(1)
def graphson_vertex_to_json_coder() -> hydra.coders.Coder[hydra.pg.graphson.syntax.Vertex, hydra.json.model.Value]:
    r"""A coder that converts GraphSON vertices to JSON. Decoding is not supported."""

    return hydra.coders.Coder((lambda _cx, v: Right(hydra.pg.graphson.coder.vertex_to_json(v))), (lambda _cx, _: Left(hydra.context.InContext(cast(hydra.errors.Error, hydra.errors.ErrorOther(hydra.errors.OtherError("decoding GraphSON JSON is currently unsupported"))), _cx))))

def vertex_property_to_graphson(encode_value: Callable[[T0], Either[T1, hydra.pg.graphson.syntax.Value]], prop: tuple[hydra.pg.model.PropertyKey, T0]) -> Either[T1, tuple[hydra.pg.graphson.syntax.PropertyKey, hydra.pg.graphson.syntax.VertexPropertyValue]]:
    r"""Convert a property graph vertex property to a GraphSON vertex property."""

    return hydra.lib.eithers.map((lambda gv: (hydra.pg.graphson.syntax.PropertyKey(hydra.lib.pairs.first(prop).value), hydra.pg.graphson.syntax.VertexPropertyValue(Nothing(), gv))), encode_value(hydra.lib.pairs.second(prop)))

def pg_vertex_with_adjacent_edges_to_graphson_vertex(encode_value: Callable[[T0], Either[T1, hydra.pg.graphson.syntax.Value]], vae: hydra.pg.model.VertexWithAdjacentEdges[T0]) -> Either[hydra.pg.graphson.syntax.Value, hydra.pg.graphson.syntax.Vertex]:
    r"""Convert a property graph vertex with adjacent edges to a GraphSON vertex."""

    @lru_cache(1)
    def vertex() -> hydra.pg.model.Vertex[T0]:
        return vae.vertex
    @lru_cache(1)
    def ins() -> frozenlist[hydra.pg.model.AdjacentEdge[T0]]:
        return vae.ins
    @lru_cache(1)
    def outs() -> frozenlist[hydra.pg.model.AdjacentEdge[T0]]:
        return vae.outs
    @lru_cache(1)
    def label() -> hydra.pg.model.VertexLabel:
        return vertex().label
    @lru_cache(1)
    def vertex_id() -> T0:
        return vertex().id
    @lru_cache(1)
    def props() -> FrozenDict[hydra.pg.model.PropertyKey, T0]:
        return vertex().properties
    return hydra.lib.eithers.bind(encode_value(vertex_id()), (lambda gid: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: vertex_property_to_graphson(encode_value, v1)), hydra.lib.maps.to_list(props())), (lambda prop_pairs: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: adjacent_edge_to_graphson(encode_value, v1)), ins()), (lambda in_pairs: hydra.lib.eithers.bind(hydra.lib.eithers.map_list((lambda v1: adjacent_edge_to_graphson(encode_value, v1)), outs()), (lambda out_pairs: Right(hydra.pg.graphson.syntax.Vertex(gid, Just(hydra.pg.graphson.syntax.VertexLabel(label().value)), aggregate_map(in_pairs), aggregate_map(out_pairs), aggregate_map(prop_pairs)))))))))))

def pg_vertex_with_adjacent_edges_to_json(encode_value: Callable[[T0], Either[T1, hydra.pg.graphson.syntax.Value]], vertex: hydra.pg.model.VertexWithAdjacentEdges[T0]) -> Either[hydra.pg.graphson.syntax.Vertex, hydra.json.model.Value]:
    r"""Convert a property graph vertex with adjacent edges to JSON."""

    return hydra.lib.eithers.bind(pg_vertex_with_adjacent_edges_to_graphson_vertex(encode_value, vertex), (lambda g_vertex: Right(hydra.pg.graphson.coder.vertex_to_json(g_vertex))))
