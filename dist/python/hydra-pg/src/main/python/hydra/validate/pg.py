# Note: this is an automatically generated file. Do not edit.

r"""Validation functions for property graphs."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.error.pg
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.pg.model

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def check_all(checks: frozenlist[Maybe[T0]]) -> Maybe[T0]:
    @lru_cache(1)
    def errors() -> frozenlist[T0]:
        return hydra.lib.maybes.cat(checks)
    return hydra.lib.lists.maybe_head(errors())

def validate_properties(check_value: Callable[[T0, T1], Maybe[hydra.error.pg.InvalidValueError]], types: frozenlist[hydra.pg.model.PropertyType[T0]], props: FrozenDict[hydra.pg.model.PropertyKey, T1]) -> Maybe[hydra.error.pg.InvalidElementPropertyError]:
    @lru_cache(1)
    def check_types() -> Maybe[hydra.error.pg.InvalidElementPropertyError]:
        return check_all(hydra.lib.lists.map((lambda x1: check_type(x1)), types))
    def check_type(t: hydra.pg.model.PropertyType[T2]) -> Maybe[hydra.error.pg.InvalidElementPropertyError]:
        return hydra.lib.logic.if_else(t.required, (lambda : hydra.lib.maybes.maybe((lambda : Just(hydra.error.pg.InvalidElementPropertyError(t.key, cast(hydra.error.pg.InvalidPropertyError, hydra.error.pg.InvalidPropertyErrorMissingRequired(t.key))))), (lambda _: Nothing()), hydra.lib.maps.lookup(t.key, props))), (lambda : Nothing()))
    @lru_cache(1)
    def check_values() -> Maybe[hydra.error.pg.InvalidElementPropertyError]:
        @lru_cache(1)
        def m() -> FrozenDict[hydra.pg.model.PropertyKey, T0]:
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (p.key, p.value)), types))
        def check_pair(pair: tuple[hydra.pg.model.PropertyKey, T1]) -> Maybe[hydra.error.pg.InvalidElementPropertyError]:
            @lru_cache(1)
            def key() -> hydra.pg.model.PropertyKey:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def val() -> T1:
                return hydra.lib.pairs.second(pair)
            return hydra.lib.maybes.maybe((lambda : Just(hydra.error.pg.InvalidElementPropertyError(key(), cast(hydra.error.pg.InvalidPropertyError, hydra.error.pg.InvalidPropertyErrorUnexpectedKey(key()))))), (lambda typ: hydra.lib.maybes.map((lambda err: hydra.error.pg.InvalidElementPropertyError(key(), cast(hydra.error.pg.InvalidPropertyError, hydra.error.pg.InvalidPropertyErrorInvalidValue(err)))), check_value(typ, val()))), hydra.lib.maps.lookup(key(), m()))
        return check_all(hydra.lib.lists.map((lambda x1: check_pair(x1)), hydra.lib.maps.to_list(props)))
    return check_all((check_types(), check_values()))

def validate_edge(check_value: Callable[[T0, T1], Maybe[hydra.error.pg.InvalidValueError]], label_for_vertex_id: Maybe[Callable[[T1], Maybe[hydra.pg.model.VertexLabel]]], typ: hydra.pg.model.EdgeType[T0], el: hydra.pg.model.Edge[T1]) -> Maybe[hydra.error.pg.InvalidEdgeError]:
    @lru_cache(1)
    def check_label() -> Maybe[hydra.error.pg.InvalidEdgeError]:
        @lru_cache(1)
        def expected() -> hydra.pg.model.EdgeLabel:
            return typ.label
        @lru_cache(1)
        def actual() -> hydra.pg.model.EdgeLabel:
            return el.label
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(actual().value, expected().value), (lambda : Nothing()), (lambda : Just(cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorLabel(hydra.error.pg.NoSuchEdgeLabelError(actual()))))))
    @lru_cache(1)
    def check_id() -> Maybe[hydra.error.pg.InvalidEdgeError]:
        return hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorId(err))), check_value(typ.id, el.id))
    @lru_cache(1)
    def check_properties() -> Maybe[hydra.error.pg.InvalidEdgeError]:
        return hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorProperty(err))), validate_properties(check_value, typ.properties, el.properties))
    @lru_cache(1)
    def check_out() -> Maybe[hydra.error.pg.InvalidEdgeError]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda f: hydra.lib.maybes.maybe((lambda : Just(cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorOutVertexNotFound()))), (lambda label: hydra.lib.logic.if_else(hydra.lib.equality.equal(label.value, typ.out.value), (lambda : Nothing()), (lambda : Just(cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorOutVertexLabel(hydra.error.pg.WrongVertexLabelError(typ.out, label))))))), f(el.out))), label_for_vertex_id)
    @lru_cache(1)
    def check_in() -> Maybe[hydra.error.pg.InvalidEdgeError]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda f: hydra.lib.maybes.maybe((lambda : Just(cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorInVertexNotFound()))), (lambda label: hydra.lib.logic.if_else(hydra.lib.equality.equal(label.value, typ.in_.value), (lambda : Nothing()), (lambda : Just(cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorInVertexLabel(hydra.error.pg.WrongVertexLabelError(typ.in_, label))))))), f(el.in_))), label_for_vertex_id)
    return check_all((check_label(), check_id(), check_properties(), check_out(), check_in()))

def validate_vertex(check_value: Callable[[T0, T1], Maybe[hydra.error.pg.InvalidValueError]], typ: hydra.pg.model.VertexType[T0], el: hydra.pg.model.Vertex[T1]) -> Maybe[hydra.error.pg.InvalidVertexError]:
    @lru_cache(1)
    def check_label() -> Maybe[hydra.error.pg.InvalidVertexError]:
        @lru_cache(1)
        def expected() -> hydra.pg.model.VertexLabel:
            return typ.label
        @lru_cache(1)
        def actual() -> hydra.pg.model.VertexLabel:
            return el.label
        return hydra.lib.logic.if_else(hydra.lib.equality.equal(actual().value, expected().value), (lambda : Nothing()), (lambda : Just(cast(hydra.error.pg.InvalidVertexError, hydra.error.pg.InvalidVertexErrorLabel(hydra.error.pg.NoSuchVertexLabelError(actual()))))))
    @lru_cache(1)
    def check_id() -> Maybe[hydra.error.pg.InvalidVertexError]:
        return hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidVertexError, hydra.error.pg.InvalidVertexErrorId(err))), check_value(typ.id, el.id))
    @lru_cache(1)
    def check_properties() -> Maybe[hydra.error.pg.InvalidVertexError]:
        return hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidVertexError, hydra.error.pg.InvalidVertexErrorProperty(err))), validate_properties(check_value, typ.properties, el.properties))
    return check_all((check_label(), check_id(), check_properties()))

def validate_graph(check_value: Callable[[T0, T1], Maybe[hydra.error.pg.InvalidValueError]], schema: hydra.pg.model.GraphSchema[T0], graph: hydra.pg.model.Graph[T1]) -> Maybe[hydra.error.pg.InvalidGraphError[T1]]:
    @lru_cache(1)
    def check_vertices() -> Maybe[hydra.error.pg.InvalidGraphError[T1]]:
        def check_vertex(el: hydra.pg.model.Vertex[T1]) -> Maybe[hydra.error.pg.InvalidGraphError[T1]]:
            return hydra.lib.maybes.maybe((lambda : Just(cast(hydra.error.pg.InvalidGraphError, hydra.error.pg.InvalidGraphErrorVertex(hydra.error.pg.InvalidGraphVertexError(el.id, cast(hydra.error.pg.InvalidVertexError, hydra.error.pg.InvalidVertexErrorLabel(hydra.error.pg.NoSuchVertexLabelError(el.label)))))))), (lambda t: hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidGraphError, hydra.error.pg.InvalidGraphErrorVertex(hydra.error.pg.InvalidGraphVertexError(el.id, err)))), validate_vertex(check_value, t, el))), hydra.lib.maps.lookup(el.label, schema.vertices))
        return check_all(hydra.lib.lists.map((lambda x1: check_vertex(x1)), hydra.lib.maps.elems(graph.vertices)))
    @lru_cache(1)
    def check_edges() -> Maybe[hydra.error.pg.InvalidGraphError[T1]]:
        def check_edge(el: hydra.pg.model.Edge[T1]) -> Maybe[hydra.error.pg.InvalidGraphError[T1]]:
            return hydra.lib.maybes.maybe((lambda : Just(cast(hydra.error.pg.InvalidGraphError, hydra.error.pg.InvalidGraphErrorEdge(hydra.error.pg.InvalidGraphEdgeError(el.id, cast(hydra.error.pg.InvalidEdgeError, hydra.error.pg.InvalidEdgeErrorLabel(hydra.error.pg.NoSuchEdgeLabelError(el.label)))))))), (lambda t: hydra.lib.maybes.map((lambda err: cast(hydra.error.pg.InvalidGraphError, hydra.error.pg.InvalidGraphErrorEdge(hydra.error.pg.InvalidGraphEdgeError(el.id, err)))), validate_edge(check_value, label_for_vertex_id(), t, el))), hydra.lib.maps.lookup(el.label, schema.edges))
        @lru_cache(1)
        def label_for_vertex_id() -> Maybe[Callable[[T1], Maybe[hydra.pg.model.VertexLabel]]]:
            return Just((lambda i: hydra.lib.maybes.map((lambda v1: v1.label), hydra.lib.maps.lookup(i, graph.vertices))))
        return check_all(hydra.lib.lists.map((lambda x1: check_edge(x1)), hydra.lib.maps.elems(graph.edges)))
    return check_all((check_vertices(), check_edges()))
