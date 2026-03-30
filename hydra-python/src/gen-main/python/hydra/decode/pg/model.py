# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.pg.model."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.pg.model

T0 = TypeVar("T0")

def edge_label(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_edge_label_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_pg_model_edge_label_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_pg_model_edge_label_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_pg_model_edge_label_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.pg.model.EdgeLabel(b)), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_pg_model_edge_label_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_edge_label_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def property_key(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_property_key_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_pg_model_property_key_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_pg_model_property_key_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_pg_model_property_key_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.pg.model.PropertyKey(b)), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_pg_model_property_key_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_property_key_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def adjacent_edge(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_adjacent_edge_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: edge_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", v, field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertex", v, field_map(), cx), (lambda field_vertex: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: property_key(x1, x2)), v, v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.model.AdjacentEdge(field_label, field_id, field_vertex, field_properties))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_adjacent_edge_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def direction(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_direction_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.model.Direction]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("out"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.Direction.OUT), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("in"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.Direction.IN), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("both"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.Direction.BOTH), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("undirected"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.Direction.UNDIRECTED), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_direction_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def edge(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_edge_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: edge_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", v, field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("out", v, field_map(), cx), (lambda field_out: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("in", v, field_map(), cx), (lambda field_in: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: property_key(x1, x2)), v, v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.model.Edge(field_label, field_id, field_out, field_in, field_properties))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_edge_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def property_type(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_property_type_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)

                        case _:
                            return Left(hydra.errors.DecodingError("expected boolean literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("key", (lambda x1, x2: property_key(x1, x2)), field_map(), cx), (lambda field_key: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", t, field_map(), cx), (lambda field_value: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("required", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_required: Right(hydra.pg.model.PropertyType(field_key, field_value, field_required))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_property_type_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def vertex_label(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_vertex_label_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_pg_model_vertex_label_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_pg_model_vertex_label_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_pg_model_vertex_label_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.pg.model.VertexLabel(b)), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_pg_model_vertex_label_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_vertex_label_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def edge_type(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_edge_type_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: edge_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", t, field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("out", (lambda x1, x2: vertex_label(x1, x2)), field_map(), cx), (lambda field_out: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("in", (lambda x1, x2: vertex_label(x1, x2)), field_map(), cx), (lambda field_in: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: property_type(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.model.EdgeType(field_label, field_id, field_out, field_in, field_properties))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_edge_type_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def vertex(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_vertex_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: vertex_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", v, field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: property_key(x1, x2)), v, v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.model.Vertex(field_label, field_id, field_properties))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_vertex_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_element_1(cx, v, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.model.Element[T0]]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("vertex"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.model.Element, hydra.pg.model.ElementVertex(t))), vertex(v, cx, input)))), (hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.model.Element, hydra.pg.model.ElementEdge(t))), edge(v, cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_element_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element_kind(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_element_kind_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.model.ElementKind]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("vertex"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.ElementKind.VERTEX), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.pg.model.ElementKind.EDGE), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_element_kind_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element_tree(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_element_tree_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("self", (lambda v12, v2: element(v, v12, v2)), field_map(), cx), (lambda field_self: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("dependencies", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: element_tree(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_dependencies: Right(hydra.pg.model.ElementTree(field_self, field_dependencies))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_element_tree_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def vertex_type(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_vertex_type_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: vertex_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", t, field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: property_type(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.model.VertexType(field_label, field_id, field_properties))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_vertex_type_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element_type(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_element_type_1(cx, t, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.model.ElementType[T0]]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("vertex"), (lambda input: hydra.lib.eithers.map((lambda t2: cast(hydra.pg.model.ElementType, hydra.pg.model.ElementTypeVertex(t2))), vertex_type(t, cx, input)))), (hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t2: cast(hydra.pg.model.ElementType, hydra.pg.model.ElementTypeEdge(t2))), edge_type(t, cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_element_type_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element_type_tree(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_element_type_tree_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("self", (lambda v12, v2: element_type(t, v12, v2)), field_map(), cx), (lambda field_self: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("dependencies", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: element_type_tree(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_dependencies: Right(hydra.pg.model.ElementTypeTree(field_self, field_dependencies))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_element_type_tree_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def graph(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_graph_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertices", (lambda v12, v2: hydra.extract.helpers.decode_map(v, (lambda v13, v22: vertex(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_vertices: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edges", (lambda v12, v2: hydra.extract.helpers.decode_map(v, (lambda v13, v22: edge(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_edges: Right(hydra.pg.model.Graph(field_vertices, field_edges))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_graph_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def graph_schema(t: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_graph_schema_1(cx, t, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertices", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: vertex_label(x1, x2)), (lambda v13, v22: vertex_type(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_vertices: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edges", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: edge_label(x1, x2)), (lambda v13, v22: edge_type(t, v13, v22)), v12, v2)), field_map(), cx), (lambda field_edges: Right(hydra.pg.model.GraphSchema(field_vertices, field_edges))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_graph_schema_1(cx, t, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def label(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_label_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.model.Label]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("vertex"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.model.Label, hydra.pg.model.LabelVertex(t))), vertex_label(cx, input)))), (hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.model.Label, hydra.pg.model.LabelEdge(t))), edge_label(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_label_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def lazy_graph(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_lazy_graph_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertices", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: vertex(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_vertices: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edges", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: edge(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_edges: Right(hydra.pg.model.LazyGraph(field_vertices, field_edges))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_lazy_graph_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def property(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_property_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("key", (lambda x1, x2: property_key(x1, x2)), field_map(), cx), (lambda field_key: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", v, field_map(), cx), (lambda field_value: Right(hydra.pg.model.Property(field_key, field_value))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_property_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def vertex_with_adjacent_edges(v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_model_vertex_with_adjacent_edges_1(cx, v, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertex", (lambda v12, v2: vertex(v, v12, v2)), field_map(), cx), (lambda field_vertex: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("ins", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: adjacent_edge(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_ins: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outs", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda v13, v22: adjacent_edge(v, v13, v22)), v12, v2)), field_map(), cx), (lambda field_outs: Right(hydra.pg.model.VertexWithAdjacentEdges(field_vertex, field_ins, field_outs))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_model_vertex_with_adjacent_edges_1(cx, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
