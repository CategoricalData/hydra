# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.compute."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def trace(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.Trace]:
    def _hoist_hydra_decode_compute_trace_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.Trace]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("stack", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_stack: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("messages", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_messages: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("other", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.term(x1, x2)), v12, v2)), field_map(), cx), (lambda field_other: Right(hydra.compute.Trace(field_stack, field_messages, field_other))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.compute.Trace"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_compute_trace_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def flow_state(s: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.FlowState[T0, T1]]:
    def _hoist_hydra_decode_compute_flow_state_1(cx: hydra.graph.Graph, s: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T2]], v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T3]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.FlowState[T2, T3]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", (lambda v12, v2: hydra.extract.helpers.decode_maybe(v, v12, v2)), field_map(), cx), (lambda field_value: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("state", s, field_map(), cx), (lambda field_state: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("trace", (lambda x1, x2: trace(x1, x2)), field_map(), cx), (lambda field_trace: Right(hydra.compute.FlowState(field_value, field_state, field_trace))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.compute.FlowState"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_compute_flow_state_1(cx, s, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
