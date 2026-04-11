# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.topology."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.topology

def vertex(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_topology_vertex_1(v1):
        match v1:
            case hydra.core.IntegerValueInt32(value=i):
                return Right(i)

            case _:
                return Left(hydra.errors.DecodingError("expected int32 value"))
    def _hoist_hydra_decode_topology_vertex_2(v1):
        match v1:
            case hydra.core.LiteralInteger(value=_match_value):
                return _hoist_hydra_decode_topology_vertex_1(_match_value)

            case _:
                return Left(hydra.errors.DecodingError("expected int32 literal"))
    def _hoist_hydra_decode_topology_vertex_3(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_topology_vertex_2(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_topology_vertex_3(stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def graph(v1: hydra.graph.Graph, v2: hydra.core.Term) -> Either[hydra.errors.DecodingError, FrozenDict[int, frozenlist[int]]]:
    return hydra.extract.core.decode_map((lambda x1, x2: vertex(x1, x2)), (lambda v12, v22: hydra.extract.core.decode_list((lambda x1, x2: vertex(x1, x2)), v12, v22)), v1, v2)

def tarjan_state(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_topology_tarjan_state_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 value"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.LiteralInteger(value=_match_value):
                            return _hoist_field_map_body_1(_match_value)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_2(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 value"))
                def _hoist_field_map_body_5(v12):
                    match v12:
                        case hydra.core.LiteralInteger(value=_match_value):
                            return _hoist_field_map_body_4(_match_value)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 literal"))
                def _hoist_field_map_body_6(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_5(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_7(v12):
                    match v12:
                        case hydra.core.IntegerValueInt32(value=i):
                            return Right(i)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 value"))
                def _hoist_field_map_body_8(v12):
                    match v12:
                        case hydra.core.LiteralInteger(value=_match_value):
                            return _hoist_field_map_body_7(_match_value)

                        case _:
                            return Left(hydra.errors.DecodingError("expected int32 literal"))
                def _hoist_field_map_body_9(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_8(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("counter", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_3(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_counter: hydra.lib.eithers.bind(hydra.extract.core.require_field("indices", (lambda v12, v2: hydra.extract.core.decode_map((lambda x1, x2: vertex(x1, x2)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_6(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_indices: hydra.lib.eithers.bind(hydra.extract.core.require_field("lowLinks", (lambda v12, v2: hydra.extract.core.decode_map((lambda x1, x2: vertex(x1, x2)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_9(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_low_links: hydra.lib.eithers.bind(hydra.extract.core.require_field("stack", (lambda v12, v2: hydra.extract.core.decode_list((lambda x1, x2: vertex(x1, x2)), v12, v2)), field_map(), cx), (lambda field_stack: hydra.lib.eithers.bind(hydra.extract.core.require_field("onStack", (lambda v12, v2: hydra.extract.core.decode_set((lambda x1, x2: vertex(x1, x2)), v12, v2)), field_map(), cx), (lambda field_on_stack: hydra.lib.eithers.bind(hydra.extract.core.require_field("sccs", (lambda v12, v2: hydra.extract.core.decode_list((lambda v13, v22: hydra.extract.core.decode_list((lambda x1, x2: vertex(x1, x2)), v13, v22)), v12, v2)), field_map(), cx), (lambda field_sccs: Right(hydra.topology.TarjanState(field_counter, field_indices, field_low_links, field_stack, field_on_stack, field_sccs))))))))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_topology_tarjan_state_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
