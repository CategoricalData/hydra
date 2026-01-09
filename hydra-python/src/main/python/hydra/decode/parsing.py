# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.parsing."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right
from typing import TypeVar, cast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.parsing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def parse_error(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseError]:
    def _hoist_hydra_decode_parsing_parse_error_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseError]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                def _hoist_body_3(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_4(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_3(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("message", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_message: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("remainder", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_4(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_remainder: Right(hydra.parsing.ParseError(field_message, field_remainder))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.parsing.ParseError"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_parsing_parse_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def parse_success(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseSuccess[T0]]:
    def _hoist_hydra_decode_parsing_parse_success_1(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseSuccess[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                field_map = hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("value", a, field_map, cx), (lambda field_value: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("remainder", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map, cx), (lambda field_remainder: Right(hydra.parsing.ParseSuccess(field_value, field_remainder))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.parsing.ParseSuccess"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_parsing_parse_success_1(a, cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def parse_result(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseResult[T0]]:
    def _hoist_hydra_decode_parsing_parse_result_1(a: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.parsing.ParseResult[T1]]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                tname = inj.type_name
                field = inj.field
                fname = field.name
                fterm = field.term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.parsing.ParseResult[T1]]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("success"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultSuccess(t))), parse_success(a, cx, input)))), (hydra.core.Name("failure"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.parsing.ParseResult, hydra.parsing.ParseResultFailure(t))), parse_error(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union type ", tname.value)))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.parsing.ParseResult"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_parsing_parse_result_1(a, cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
