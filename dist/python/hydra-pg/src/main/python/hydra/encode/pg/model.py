# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.pg.model."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.lib.lists
import hydra.lib.maps
import hydra.pg.model

T0 = TypeVar("T0")

def edge_label(x: hydra.pg.model.EdgeLabel) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.pg.model.EdgeLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def property_key(x: hydra.pg.model.PropertyKey) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.pg.model.PropertyKey"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def adjacent_edge(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.AdjacentEdge[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.AdjacentEdge"), (hydra.core.Field(hydra.core.Name("label"), edge_label(x.label)), hydra.core.Field(hydra.core.Name("id"), v(x.id)), hydra.core.Field(hydra.core.Name("vertex"), v(x.vertex)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: property_key(x1)), v, x.properties))))))))

def direction(v1: hydra.pg.model.Direction) -> hydra.core.Term:
    match v1:
        case hydra.pg.model.Direction.OUT:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Direction"), hydra.core.Field(hydra.core.Name("out"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.pg.model.Direction.IN:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Direction"), hydra.core.Field(hydra.core.Name("in"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.pg.model.Direction.BOTH:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Direction"), hydra.core.Field(hydra.core.Name("both"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.pg.model.Direction.UNDIRECTED:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Direction"), hydra.core.Field(hydra.core.Name("undirected"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def edge(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.Edge[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.Edge"), (hydra.core.Field(hydra.core.Name("label"), edge_label(x.label)), hydra.core.Field(hydra.core.Name("id"), v(x.id)), hydra.core.Field(hydra.core.Name("out"), v(x.out)), hydra.core.Field(hydra.core.Name("in"), v(x.in_)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: property_key(x1)), v, x.properties))))))))

def property_type(t: Callable[[T0], hydra.core.Term], x: hydra.pg.model.PropertyType[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.PropertyType"), (hydra.core.Field(hydra.core.Name("key"), property_key(x.key)), hydra.core.Field(hydra.core.Name("value"), t(x.value)), hydra.core.Field(hydra.core.Name("required"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.required)))))))))

def vertex_label(x: hydra.pg.model.VertexLabel) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.pg.model.VertexLabel"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def edge_type(t: Callable[[T0], hydra.core.Term], x: hydra.pg.model.EdgeType[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.EdgeType"), (hydra.core.Field(hydra.core.Name("label"), edge_label(x.label)), hydra.core.Field(hydra.core.Name("id"), t(x.id)), hydra.core.Field(hydra.core.Name("out"), vertex_label(x.out)), hydra.core.Field(hydra.core.Name("in"), vertex_label(x.in_)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: property_type(t, v1)), x.properties))))))))

def vertex(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.Vertex[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.Vertex"), (hydra.core.Field(hydra.core.Name("label"), vertex_label(x.label)), hydra.core.Field(hydra.core.Name("id"), v(x.id)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: property_key(x1)), v, x.properties))))))))

def element(v: Callable[[T0], hydra.core.Term]):
    def _hoist_hydra_encode_pg_model_element_1(v, v1):
        match v1:
            case hydra.pg.model.ElementVertex(value=y):
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Element"), hydra.core.Field(hydra.core.Name("vertex"), vertex(v, y)))))

            case hydra.pg.model.ElementEdge(value=y):
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Element"), hydra.core.Field(hydra.core.Name("edge"), edge(v, y)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return _hoist_hydra_encode_pg_model_element_1(v)

def element_kind(v1: hydra.pg.model.ElementKind) -> hydra.core.Term:
    match v1:
        case hydra.pg.model.ElementKind.VERTEX:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.ElementKind"), hydra.core.Field(hydra.core.Name("vertex"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case hydra.pg.model.ElementKind.EDGE:
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.ElementKind"), hydra.core.Field(hydra.core.Name("edge"), cast(hydra.core.Term, hydra.core.TermUnit())))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def element_tree(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.ElementTree[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.ElementTree"), (hydra.core.Field(hydra.core.Name("self"), element(v, x.self)), hydra.core.Field(hydra.core.Name("dependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: element_tree(v, v1)), x.dependencies))))))))

def vertex_type(t: Callable[[T0], hydra.core.Term], x: hydra.pg.model.VertexType[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.VertexType"), (hydra.core.Field(hydra.core.Name("label"), vertex_label(x.label)), hydra.core.Field(hydra.core.Name("id"), t(x.id)), hydra.core.Field(hydra.core.Name("properties"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: property_type(t, v1)), x.properties))))))))

def element_type(t: Callable[[T0], hydra.core.Term]):
    def _hoist_hydra_encode_pg_model_element_type_1(t, v1):
        match v1:
            case hydra.pg.model.ElementTypeVertex(value=y):
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.ElementType"), hydra.core.Field(hydra.core.Name("vertex"), vertex_type(t, y)))))

            case hydra.pg.model.ElementTypeEdge(value=y):
                return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.ElementType"), hydra.core.Field(hydra.core.Name("edge"), edge_type(t, y)))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return _hoist_hydra_encode_pg_model_element_type_1(t)

def element_type_tree(t: Callable[[T0], hydra.core.Term], x: hydra.pg.model.ElementTypeTree[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.ElementTypeTree"), (hydra.core.Field(hydra.core.Name("self"), element_type(t, x.self)), hydra.core.Field(hydra.core.Name("dependencies"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: element_type_tree(t, v1)), x.dependencies))))))))

def graph(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.Graph[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.Graph"), (hydra.core.Field(hydra.core.Name("vertices"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(v, (lambda v1: vertex(v, v1)), x.vertices)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap(v, (lambda v1: edge(v, v1)), x.edges))))))))

def graph_schema(t: Callable[[T0], hydra.core.Term], x: hydra.pg.model.GraphSchema[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.GraphSchema"), (hydra.core.Field(hydra.core.Name("vertices"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: vertex_label(x1)), (lambda v1: vertex_type(t, v1)), x.vertices)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermMap(hydra.lib.maps.bimap((lambda x1: edge_label(x1)), (lambda v1: edge_type(t, v1)), x.edges))))))))

def label(v1: hydra.pg.model.Label) -> hydra.core.Term:
    match v1:
        case hydra.pg.model.LabelVertex(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Label"), hydra.core.Field(hydra.core.Name("vertex"), vertex_label(y)))))

        case hydra.pg.model.LabelEdge(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.pg.model.Label"), hydra.core.Field(hydra.core.Name("edge"), edge_label(y2)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def lazy_graph(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.LazyGraph[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.LazyGraph"), (hydra.core.Field(hydra.core.Name("vertices"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: vertex(v, v1)), x.vertices)))), hydra.core.Field(hydra.core.Name("edges"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: edge(v, v1)), x.edges))))))))

def property(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.Property[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.Property"), (hydra.core.Field(hydra.core.Name("key"), property_key(x.key)), hydra.core.Field(hydra.core.Name("value"), v(x.value))))))

def vertex_with_adjacent_edges(v: Callable[[T0], hydra.core.Term], x: hydra.pg.model.VertexWithAdjacentEdges[T0]) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.pg.model.VertexWithAdjacentEdges"), (hydra.core.Field(hydra.core.Name("vertex"), vertex(v, x.vertex)), hydra.core.Field(hydra.core.Name("ins"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: adjacent_edge(v, v1)), x.ins)))), hydra.core.Field(hydra.core.Name("outs"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda v1: adjacent_edge(v, v1)), x.outs))))))))
