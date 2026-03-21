# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.context."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.context
import hydra.core
import hydra.decode.core
import hydra.errors
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers

T0 = TypeVar("T0")

def context(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_context_context_1(cx, v1):
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
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("trace", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_trace: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("messages", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), v12, v2)), field_map(), cx), (lambda field_messages: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("other", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.term(x1, x2)), v12, v2)), field_map(), cx), (lambda field_other: Right(hydra.context.Context(field_trace, field_messages, field_other))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_context_context_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def in_context(e: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_context_in_context_1(cx, e, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("object", e, field_map(), cx), (lambda field_object: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("context", (lambda x1, x2: context(x1, x2)), field_map(), cx), (lambda field_context: Right(hydra.context.InContext(field_object, field_context))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_context_in_context_1(cx, e, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
