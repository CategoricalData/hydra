# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.topology."""

from __future__ import annotations
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.topology
import hydra.util

def vertex(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
    def _hoist_hydra_decode_topology_vertex_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.IntegerValueInt32(value=i):
                return Right(i)
            
            case _:
                return Left(hydra.util.DecodingError("expected int32 value"))
    def _hoist_hydra_decode_topology_vertex_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.LiteralInteger(value=v1):
                return _hoist_hydra_decode_topology_vertex_1(v1)
            
            case _:
                return Left(hydra.util.DecodingError("expected int32 literal"))
    def _hoist_hydra_decode_topology_vertex_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_topology_vertex_2(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_topology_vertex_3(stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def graph(v1: hydra.graph.Graph, v2: hydra.core.Term) -> Either[hydra.util.DecodingError, FrozenDict[int, frozenlist[int]]]:
    return hydra.extract.helpers.decode_map(vertex, (lambda v1, v2: hydra.extract.helpers.decode_list(vertex, v1, v2)), v1, v2)

def tarjan_state(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.topology.TarjanState]:
    def _hoist_hydra_decode_topology_tarjan_state_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.topology.TarjanState]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.LiteralInteger(value=v1):
                            return _hoist_body_1(v1)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_2(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_4(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_5(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.LiteralInteger(value=v1):
                            return _hoist_body_4(v1)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_6(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_5(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_7(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 value"))
                def _hoist_body_8(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.LiteralInteger(value=v1):
                            return _hoist_body_7(v1)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected int32 literal"))
                def _hoist_body_9(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_8(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("counter", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_counter: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("indices", (lambda v1, v2: hydra.extract.helpers.decode_map(vertex, (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_6(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_indices: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("lowLinks", (lambda v1, v2: hydra.extract.helpers.decode_map(vertex, (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_9(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_low_links: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("stack", (lambda v1, v2: hydra.extract.helpers.decode_list(vertex, v1, v2)), field_map, cx), (lambda field_stack: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("onStack", (lambda v1, v2: hydra.extract.helpers.decode_set(vertex, v1, v2)), field_map, cx), (lambda field_on_stack: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("sccs", (lambda v1, v2: hydra.extract.helpers.decode_list((lambda v1, v2: hydra.extract.helpers.decode_list(vertex, v1, v2)), v1, v2)), field_map, cx), (lambda field_sccs: Right(hydra.topology.TarjanState(field_counter, field_indices, field_low_links, field_stack, field_on_stack, field_sccs))))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.topology.TarjanState"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_topology_tarjan_state_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
