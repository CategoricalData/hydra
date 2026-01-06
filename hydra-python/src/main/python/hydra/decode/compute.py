# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.compute."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.compute
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.graph
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
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                def _hoist_body_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return cast(Either[hydra.util.DecodingError, str], Right(s))
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected string literal")))
                def _hoist_body_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError("expected literal")))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("stack", (lambda v1, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_stack: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("messages", (lambda v1, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, str], Left(hydra.util.DecodingError(err)))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v1, v2)), field_map, cx), (lambda field_messages: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("other", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.term, v1, v2)), field_map, cx), (lambda field_other: cast(Either[hydra.util.DecodingError, hydra.compute.Trace], Right(hydra.compute.Trace(field_stack, field_messages, field_other)))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.compute.Trace], Left(hydra.util.DecodingError("expected record of type hydra.compute.Trace")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.compute.Trace], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_compute_trace_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def flow_state(s: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.FlowState[T0, T1]]:
    def _hoist_hydra_decode_compute_flow_state_1(cx: hydra.graph.Graph, s: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T2]], v: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T3]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.compute.FlowState[T2, T3]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", (lambda v1, v2: hydra.extract.helpers.decode_maybe(v, v1, v2)), field_map, cx), (lambda field_value: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("state", s, field_map, cx), (lambda field_state: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("trace", trace, field_map, cx), (lambda field_trace: cast(Either[hydra.util.DecodingError, hydra.compute.FlowState[T2, T3]], Right(cast(hydra.compute.FlowState[T2, T3], hydra.compute.FlowState(field_value, field_state, field_trace))))))))))
            
            case _:
                return cast(Either[hydra.util.DecodingError, hydra.compute.FlowState[T2, T3]], Left(hydra.util.DecodingError("expected record of type hydra.compute.FlowState")))
    return hydra.lib.eithers.either((lambda err: cast(Either[hydra.util.DecodingError, hydra.compute.FlowState[T0, T1]], Left(hydra.util.DecodingError(err)))), (lambda stripped: _hoist_hydra_decode_compute_flow_state_1(cx, s, v, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
