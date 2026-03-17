# Note: this is an automatically generated file. Do not edit.

r"""Utilities for validating property graphs against property graph schemas."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, Just, Maybe, Nothing, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.strings
import hydra.pg.model

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def check_all(checks: frozenlist[Maybe[T0]]) -> Maybe[T0]:
    @lru_cache(1)
    def errors() -> frozenlist[T0]:
        return hydra.lib.maybes.cat(checks)
    return hydra.lib.lists.safe_head(errors())

def prepend(prefix: str, msg: str) -> str:
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(prefix, ": "), msg)

def edge_error(show_value: Callable[[T0], str], e: hydra.pg.model.Edge[T0], v1: str) -> str:
    return prepend(hydra.lib.strings.cat2("Invalid edge with id ", show_value(e.id)), v1)

def edge_label_mismatch(expected: hydra.pg.model.EdgeLabel, actual: hydra.pg.model.EdgeLabel) -> str:
    return hydra.lib.strings.cat2(hydra.lib.strings.cat2(hydra.lib.strings.cat2("expected ", expected.value), ", found "), actual.value)

def validate_properties(check_value: Callable[[T0, T1], Maybe[str]], types: frozenlist[hydra.pg.model.PropertyType[T0]], props: FrozenDict[hydra.pg.model.PropertyKey, T1]) -> Maybe[str]:
    @lru_cache(1)
    def check_types() -> Maybe[str]:
        return check_all(hydra.lib.lists.map((lambda x1: check_type(x1)), types))
    def check_type(t: hydra.pg.model.PropertyType[T2]) -> Maybe[str]:
        return hydra.lib.logic.if_else(t.required, (lambda : hydra.lib.maybes.maybe((lambda : Just(prepend("Missing value for ", t.key.value))), (lambda _: Nothing()), hydra.lib.maps.lookup(t.key, props))), (lambda : Nothing()))
    @lru_cache(1)
    def check_values() -> Maybe[str]:
        @lru_cache(1)
        def m() -> FrozenDict[hydra.pg.model.PropertyKey, T0]:
            return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda p: (p.key, p.value)), types))
        def check_pair(pair: tuple[hydra.pg.model.PropertyKey, T1]) -> Maybe[str]:
            @lru_cache(1)
            def key() -> hydra.pg.model.PropertyKey:
                return hydra.lib.pairs.first(pair)
            @lru_cache(1)
            def val() -> T1:
                return hydra.lib.pairs.second(pair)
            return hydra.lib.maybes.maybe((lambda : Just(prepend("Unexpected key", key().value))), (lambda typ: hydra.lib.maybes.map((lambda v1: prepend("Invalid value", v1)), check_value(typ, val()))), hydra.lib.maps.lookup(key(), m()))
        return check_all(hydra.lib.lists.map((lambda x1: check_pair(x1)), hydra.lib.maps.to_list(props)))
    return check_all((check_types(), check_values()))

def verify(b: bool, err: T0) -> Maybe[T0]:
    return hydra.lib.logic.if_else(b, (lambda : Nothing()), (lambda : Just(err)))

def vertex_label_mismatch(expected: hydra.pg.model.VertexLabel, actual: hydra.pg.model.VertexLabel) -> str:
    return hydra.lib.strings.cat(("expected ", expected.value, ", found ", actual.value))

def validate_edge(check_value: Callable[[T0, T1], Maybe[str]], show_value: Callable[[T1], str], label_for_vertex_id: Maybe[Callable[[T1], Maybe[hydra.pg.model.VertexLabel]]], typ: hydra.pg.model.EdgeType[T0], el: hydra.pg.model.Edge[T1]) -> Maybe[str]:
    def fail_with(v1: str) -> str:
        return edge_error(show_value, el, v1)
    @lru_cache(1)
    def check_label() -> Maybe[str]:
        @lru_cache(1)
        def expected() -> hydra.pg.model.EdgeLabel:
            return typ.label
        @lru_cache(1)
        def actual() -> hydra.pg.model.EdgeLabel:
            return el.label
        return verify(hydra.lib.equality.equal(actual().value, expected().value), fail_with(prepend("Wrong label", edge_label_mismatch(expected(), actual()))))
    @lru_cache(1)
    def check_id() -> Maybe[str]:
        return hydra.lib.maybes.map((lambda arg_: fail_with(prepend("Invalid id", arg_))), check_value(typ.id, el.id))
    @lru_cache(1)
    def check_properties() -> Maybe[str]:
        return hydra.lib.maybes.map((lambda arg_: fail_with(prepend("Invalid property", arg_))), validate_properties(check_value, typ.properties, el.properties))
    @lru_cache(1)
    def check_out() -> Maybe[str]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda f: hydra.lib.maybes.maybe((lambda : Just(fail_with(prepend("Out-vertex does not exist", show_value(el.out))))), (lambda label: verify(hydra.lib.equality.equal(label.value, typ.out.value), fail_with(prepend("Wrong out-vertex label", vertex_label_mismatch(typ.out, label))))), f(el.out))), label_for_vertex_id)
    @lru_cache(1)
    def check_in() -> Maybe[str]:
        return hydra.lib.maybes.maybe((lambda : Nothing()), (lambda f: hydra.lib.maybes.maybe((lambda : Just(fail_with(prepend("In-vertex does not exist", show_value(el.in_))))), (lambda label: verify(hydra.lib.equality.equal(label.value, typ.in_.value), fail_with(prepend("Wrong in-vertex label", vertex_label_mismatch(typ.in_, label))))), f(el.in_))), label_for_vertex_id)
    return check_all((check_label(), check_id(), check_properties(), check_out(), check_in()))

def vertex_error(show_value: Callable[[T0], str], v: hydra.pg.model.Vertex[T0], v1: str) -> str:
    return prepend(hydra.lib.strings.cat2("Invalid vertex with id ", show_value(v.id)), v1)

def validate_vertex(check_value: Callable[[T0, T1], Maybe[str]], show_value: Callable[[T1], str], typ: hydra.pg.model.VertexType[T0], el: hydra.pg.model.Vertex[T1]) -> Maybe[str]:
    def fail_with(v1: str) -> str:
        return vertex_error(show_value, el, v1)
    @lru_cache(1)
    def check_label() -> Maybe[str]:
        @lru_cache(1)
        def expected() -> hydra.pg.model.VertexLabel:
            return typ.label
        @lru_cache(1)
        def actual() -> hydra.pg.model.VertexLabel:
            return el.label
        return verify(hydra.lib.equality.equal(actual().value, expected().value), fail_with(prepend("Wrong label", vertex_label_mismatch(expected(), actual()))))
    @lru_cache(1)
    def check_id() -> Maybe[str]:
        return hydra.lib.maybes.map((lambda arg_: fail_with(prepend("Invalid id", arg_))), check_value(typ.id, el.id))
    @lru_cache(1)
    def check_properties() -> Maybe[str]:
        return hydra.lib.maybes.map((lambda arg_: fail_with(prepend("Invalid property", arg_))), validate_properties(check_value, typ.properties, el.properties))
    return check_all((check_label(), check_id(), check_properties()))

def validate_element(check_value: Callable[[T0, T1], Maybe[str]], show_value: Callable[[T1], str], label_for_vertex_id: Maybe[Callable[[T1], Maybe[hydra.pg.model.VertexLabel]]], typ: hydra.pg.model.ElementType[T0], el: hydra.pg.model.Element[T1]):
    def _hoist_hydra_pg_validation_validate_element_1(check_value, show_value, vt, v1):
        match v1:
            case hydra.pg.model.ElementEdge(value=e):
                return Just(prepend("Edge instead of vertex", show_value(e.id)))

            case hydra.pg.model.ElementVertex(value=vertex):
                return validate_vertex(check_value, show_value, vt, vertex)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_pg_validation_validate_element_2(check_value, et, label_for_vertex_id, show_value, v1):
        match v1:
            case hydra.pg.model.ElementVertex(value=v):
                return Just(prepend("Vertex instead of edge", show_value(v.id)))

            case hydra.pg.model.ElementEdge(value=edge):
                return validate_edge(check_value, show_value, label_for_vertex_id, et, edge)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_pg_validation_validate_element_3(check_value, el, label_for_vertex_id, show_value, v1):
        match v1:
            case hydra.pg.model.ElementTypeVertex(value=vt):
                return _hoist_hydra_pg_validation_validate_element_1(check_value, show_value, vt, el)

            case hydra.pg.model.ElementTypeEdge(value=et):
                return _hoist_hydra_pg_validation_validate_element_2(check_value, et, label_for_vertex_id, show_value, el)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return _hoist_hydra_pg_validation_validate_element_3(check_value, el, label_for_vertex_id, show_value, typ)

def validate_graph(check_value: Callable[[T0, T1], Maybe[str]], show_value: Callable[[T1], str], schema: hydra.pg.model.GraphSchema[T0], graph: hydra.pg.model.Graph[T1]) -> Maybe[str]:
    @lru_cache(1)
    def check_vertices() -> Maybe[str]:
        def check_vertex(el: hydra.pg.model.Vertex[T1]) -> Maybe[str]:
            return hydra.lib.maybes.maybe((lambda : Just(vertex_error(show_value, el, prepend("Unexpected label", el.label.value)))), (lambda t: validate_vertex(check_value, show_value, t, el)), hydra.lib.maps.lookup(el.label, schema.vertices))
        return check_all(hydra.lib.lists.map((lambda x1: check_vertex(x1)), hydra.lib.maps.elems(graph.vertices)))
    @lru_cache(1)
    def check_edges() -> Maybe[str]:
        def check_edge(el: hydra.pg.model.Edge[T1]) -> Maybe[str]:
            return hydra.lib.maybes.maybe((lambda : Just(edge_error(show_value, el, prepend("Unexpected label", el.label.value)))), (lambda t: validate_edge(check_value, show_value, label_for_vertex_id(), t, el)), hydra.lib.maps.lookup(el.label, schema.edges))
        @lru_cache(1)
        def label_for_vertex_id() -> Maybe[Callable[[T1], Maybe[hydra.pg.model.VertexLabel]]]:
            return Just((lambda i: hydra.lib.maybes.map((lambda v1: v1.label), hydra.lib.maps.lookup(i, graph.vertices))))
        return check_all(hydra.lib.lists.map((lambda x1: check_edge(x1)), hydra.lib.maps.elems(graph.edges)))
    return check_all((check_vertices(), check_edges()))
