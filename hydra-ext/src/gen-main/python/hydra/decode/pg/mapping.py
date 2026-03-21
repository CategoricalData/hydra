# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.pg.mapping."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.decode.pg.model
import hydra.errors
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.pg.mapping

def annotation_schema(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_annotation_schema_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_3(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_5(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_6(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_7(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_8(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_7(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_9(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_10(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_9(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_11(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_12(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_11(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_13(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_14(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_13(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_15(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_16(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_15(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_17(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_18(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_17(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_19(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_20(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_19(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_21(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_22(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_21(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_23(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_24(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_23(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_25(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_26(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_25(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_27(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_28(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_27(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_body_29(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_body_30(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_29(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertexLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_vertex_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edgeLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_edge_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("vertexId", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_vertex_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("edgeId", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_8(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_edge_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("propertyKey", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_10(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_property_key: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("propertyValue", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_12(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_property_value: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outVertex", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_14(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_out_vertex: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outVertexLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_16(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_out_vertex_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inVertex", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_18(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_in_vertex: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inVertexLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_20(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_in_vertex_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outEdge", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_22(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_out_edge: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("outEdgeLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_24(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_out_edge_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inEdge", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_26(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_in_edge: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inEdgeLabel", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_28(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_in_edge_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("ignore", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_30(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_ignore: Right(hydra.pg.mapping.AnnotationSchema(field_vertex_label, field_edge_label, field_vertex_id, field_edge_id, field_property_key, field_property_value, field_out_vertex, field_out_vertex_label, field_in_vertex, field_in_vertex_label, field_out_edge, field_out_edge_label, field_in_edge, field_in_edge_label, field_ignore))))))))))))))))))))))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_annotation_schema_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def value_spec(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_value_spec_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map():
                    def _hoist_variant_map_1(v12):
                        match v12:
                            case hydra.core.LiteralString(value=s):
                                return Right(s)

                            case _:
                                return Left(hydra.errors.DecodingError("expected string literal"))
                    def _hoist_variant_map_2(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_1(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("value"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecValue())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("pattern"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.mapping.ValueSpec, hydra.pg.mapping.ValueSpecPattern(t))), hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_variant_map_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx, input)))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_value_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def property_spec(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_property_spec_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("key", (lambda x1, x2: hydra.decode.pg.model.property_key(x1, x2)), field_map(), cx), (lambda field_key: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", (lambda x1, x2: value_spec(x1, x2)), field_map(), cx), (lambda field_value: Right(hydra.pg.mapping.PropertySpec(field_key, field_value))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_property_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def edge_spec(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_edge_spec_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: hydra.decode.pg.model.edge_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", (lambda x1, x2: value_spec(x1, x2)), field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("out", (lambda x1, x2: value_spec(x1, x2)), field_map(), cx), (lambda field_out: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("in", (lambda x1, x2: value_spec(x1, x2)), field_map(), cx), (lambda field_in: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: property_spec(x1, x2)), v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.mapping.EdgeSpec(field_label, field_id, field_out, field_in, field_properties))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_edge_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def vertex_spec(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_vertex_spec_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("label", (lambda x1, x2: hydra.decode.pg.model.vertex_label(x1, x2)), field_map(), cx), (lambda field_label: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("id", (lambda x1, x2: value_spec(x1, x2)), field_map(), cx), (lambda field_id: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("properties", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: property_spec(x1, x2)), v12, v2)), field_map(), cx), (lambda field_properties: Right(hydra.pg.mapping.VertexSpec(field_label, field_id, field_properties))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_vertex_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def element_spec(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_pg_mapping_element_spec_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.pg.mapping.ElementSpec]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("vertex"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.mapping.ElementSpec, hydra.pg.mapping.ElementSpecVertex(t))), vertex_spec(cx, input)))), (hydra.core.Name("edge"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.pg.mapping.ElementSpec, hydra.pg.mapping.ElementSpecEdge(t))), edge_spec(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_pg_mapping_element_spec_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
