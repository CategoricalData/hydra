# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.parsing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsing

T0 = TypeVar("T0")

def parse_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_parsing_parse_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("message", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_message: hydra.lib.eithers.bind(hydra.extract.core.require_field("remainder", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_remainder: Right(hydra.parsing.ParseError(field_message, field_remainder))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_parsing_parse_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def parse_success(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_parsing_parse_success_1(a, cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("value", a, field_map(), cx), (lambda field_value: hydra.lib.eithers.bind(hydra.extract.core.require_field("remainder", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_remainder: Right(hydra.parsing.ParseSuccess(field_value, field_remainder))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_parsing_parse_success_1(a, cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def parse_result(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_parsing_parse_result_1(a, cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.parsing.ParseResult[T0]]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("success"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(t))), parse_success(a, cx, input)))), (hydra.core.Name("failure"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(t))), parse_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_parsing_parse_result_1(a, cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
