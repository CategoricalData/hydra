# Note: this is an automatically generated file. Do not edit.

r"""Utility functions for property graph operations."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Just, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.extract.core
import hydra.graph
import hydra.json.model
import hydra.lib.eithers
import hydra.lib.lists
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.pg.coder
import hydra.pg.mapping
import hydra.pg.model
import hydra.show.core
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

# Default Tinkerpop annotation schema.
default_tinkerpop_annotations = hydra.pg.mapping.AnnotationSchema("vertexLabel", "edgeLabel", "vertexId", "edgeId", "key", "value", "outVertex", "outVertexLabel", "inVertex", "inVertexLabel", "outEdge", "outEdgeLabel", "inEdge", "inEdgeLabel", "ignore")

def exp_string(cx: hydra.context.Context, term: hydra.core.Term) -> Either[hydra.context.InContext[hydra.error.Error], str]:
    r"""Extract a string from a term using the empty graph."""

    return hydra.extract.core.string(cx, hydra.graph.Graph(hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.maps.empty(), hydra.lib.sets.empty()), term)

@lru_cache(1)
def example_pg_schema() -> hydra.pg.mapping.Schema[T0, None, str]:
    r"""Example property graph schema with string values."""

    return hydra.pg.mapping.Schema(hydra.util.Coder((lambda _, _2: Right(None)), (lambda _, _2: Right(cast(hydra.core.Type, hydra.core.TypeUnit())))), hydra.util.Coder((lambda cx, t: exp_string(cx, t)), (lambda _cx, s: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))))), hydra.util.Coder((lambda _, _2: Right(None)), (lambda _, _2: Right(cast(hydra.core.Type, hydra.core.TypeUnit())))), hydra.util.Coder((lambda cx, t: exp_string(cx, t)), (lambda _cx, s: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))))), hydra.util.Coder((lambda _, _2: Right(None)), (lambda _, _2: Right(cast(hydra.core.Type, hydra.core.TypeUnit())))), hydra.util.Coder((lambda cx, t: exp_string(cx, t)), (lambda _cx, s: Right(cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(s))))))), default_tinkerpop_annotations, "defaultVertexId", "defaultEdgeId")

def lazy_graph_to_elements(lg: hydra.pg.model.LazyGraph[T0]) -> frozenlist[hydra.pg.model.Element[T0]]:
    r"""Get all elements from a lazy graph."""

    return hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x: cast(hydra.pg.model.Element, hydra.pg.model.ElementVertex(x))), lg.vertices), hydra.lib.lists.map((lambda x: cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(x))), lg.edges))

def pg_element_to_json(schema: hydra.pg.mapping.Schema[T0, T1, T2], el: hydra.pg.model.Element[T2], cx: hydra.context.Context):
    def _hoist_hydra_pg_utils_pg_element_to_json_1(cx, schema, v1):
        match v1:
            case hydra.pg.model.ElementVertex(value=vertex):
                return hydra.lib.eithers.bind(schema.vertex_ids.decode(cx, vertex.id), (lambda term: (label_json := cast(hydra.json.model.Value, hydra.json.model.ValueString(vertex.label.value)), hydra.lib.eithers.map((lambda props_json: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat((Just(("label", label_json)), Just(("id", cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term))))), props_json)))))), hydra.lib.logic.if_else(hydra.lib.maps.null(vertex.properties), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.map((lambda p: Just(("properties", cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(p)))))), hydra.lib.eithers.map_list((lambda pair: (key := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.eithers.bind(schema.property_values.decode(cx, v), (lambda term2: Right((key.value, cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term2))))))))[2]), hydra.lib.maps.to_list(vertex.properties)))))))[1]))

            case hydra.pg.model.ElementEdge(value=edge):
                return hydra.lib.eithers.bind(schema.edge_ids.decode(cx, edge.id), (lambda term: hydra.lib.eithers.bind(schema.vertex_ids.decode(cx, edge.out), (lambda term_out: hydra.lib.eithers.bind(schema.vertex_ids.decode(cx, edge.in_), (lambda term_in: (label_json := cast(hydra.json.model.Value, hydra.json.model.ValueString(edge.label.value)), hydra.lib.eithers.map((lambda props_json: cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(hydra.lib.maybes.cat((Just(("label", label_json)), Just(("id", cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term))))), Just(("out", cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term_out))))), Just(("in", cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term_in))))), props_json)))))), hydra.lib.logic.if_else(hydra.lib.maps.null(edge.properties), (lambda : Right(Nothing())), (lambda : hydra.lib.eithers.map((lambda p: Just(("properties", cast(hydra.json.model.Value, hydra.json.model.ValueObject(hydra.lib.maps.from_list(p)))))), hydra.lib.eithers.map_list((lambda pair: (key := hydra.lib.pairs.first(pair), v := hydra.lib.pairs.second(pair), hydra.lib.eithers.bind(schema.property_values.decode(cx, v), (lambda term2: Right((key.value, cast(hydra.json.model.Value, hydra.json.model.ValueString(hydra.show.core.term(term2))))))))[2]), hydra.lib.maps.to_list(edge.properties)))))))[1]))))))

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return _hoist_hydra_pg_utils_pg_element_to_json_1(cx, schema, el)

def pg_elements_to_json(schema: hydra.pg.mapping.Schema[T0, T1, T2], els: frozenlist[hydra.pg.model.Element[T2]], cx: hydra.context.Context) -> Either[hydra.context.InContext[hydra.error.Error], hydra.json.model.Value]:
    r"""Convert a list of property graph elements to JSON."""

    return hydra.lib.eithers.map((lambda els_: cast(hydra.json.model.Value, hydra.json.model.ValueArray(els_))), hydra.lib.eithers.map_list((lambda el: pg_element_to_json(schema, el, cx)), els))

def property_graph_elements(g: hydra.pg.model.Graph[T0]) -> frozenlist[hydra.pg.model.Element[T0]]:
    r"""Get all elements from a property graph."""

    return hydra.lib.lists.concat2(hydra.lib.lists.map((lambda x: cast(hydra.pg.model.Element, hydra.pg.model.ElementVertex(x))), hydra.lib.maps.elems(g.vertices)), hydra.lib.lists.map((lambda x: cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(x))), hydra.lib.maps.elems(g.edges)))

def type_application_term_to_property_graph(schema: hydra.pg.mapping.Schema[T0, T1, T2], typ: hydra.core.Type, vid_type: T1, eid_type: T1, cx: hydra.context.Context, g: hydra.graph.Graph) -> Either[hydra.context.InContext[hydra.error.Error], Callable[[hydra.core.Term, hydra.context.Context], Either[hydra.context.InContext[hydra.error.Error], frozenlist[hydra.pg.model.Element[T2]]]]]:
    r"""Convert a type-annotated term to property graph elements."""

    return hydra.lib.eithers.bind(hydra.pg.coder.element_coder(Nothing(), schema, typ, vid_type, eid_type, cx, g), (lambda adapter: Right((lambda term, cx_: hydra.lib.eithers.map((lambda tree: (flatten_tree := (lambda t: hydra.lib.lists.cons(t.self, hydra.lib.lists.concat(hydra.lib.lists.map((lambda x1: flatten_tree(x1)), t.dependencies)))), flatten_tree(tree))[1]), adapter.coder.encode(cx_, term))))))
