# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.util."""

from __future__ import annotations
from collections.abc import Callable
from hydra.dsl.python import Either, FrozenDict, Left, Right
from typing import cast
import hydra.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def case_convention(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.CaseConvention]:
    def _hoist_hydra_decode_util_case_convention_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.CaseConvention]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.util.CaseConvention]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("camel"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.CAMEL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("pascal"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.PASCAL), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("lowerSnake"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.LOWER_SNAKE), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("upperSnake"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.UPPER_SNAKE), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.util.CaseConvention"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_util_case_convention_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def comparison(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.Comparison]:
    def _hoist_hydra_decode_util_comparison_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.Comparison]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.util.Comparison]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("lessThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.LESS_THAN), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("equalTo"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.EQUAL_TO), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("greaterThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.GREATER_THAN), hydra.extract.helpers.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.util.Comparison"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_util_comparison_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def decoding_error(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.DecodingError]:
    def _hoist_hydra_decode_util_decoding_error_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)
            
            case _:
                return Left(hydra.util.DecodingError("expected string literal"))
    def _hoist_hydra_decode_util_decoding_error_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_util_decoding_error_1(v)
            
            case _:
                return Left(hydra.util.DecodingError("expected literal"))
    def _hoist_hydra_decode_util_decoding_error_3(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.DecodingError]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.util.DecodingError(b)), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_hydra_decode_util_decoding_error_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.util.DecodingError"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_util_decoding_error_3(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def precision(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.Precision]:
    def _hoist_hydra_decode_util_precision_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.util.Precision]:
        match v1:
            case hydra.core.TermUnion(value=inj):
                def tname() -> hydra.core.Type:
                    return inj.type_name
                def field() -> hydra.core.Type:
                    return inj.field
                def fname() -> hydra.core.Type:
                    return field().name
                def fterm() -> hydra.core.Type:
                    return field().term
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.util.DecodingError, hydra.util.Precision]]]:
                    def _hoist_variant_map_1(v1: hydra.core.IntegerValue) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 value"))
                    def _hoist_variant_map_2(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.LiteralInteger(value=v1):
                                return _hoist_variant_map_1(v1)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_3(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, int]:
                        match v1:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)
                            
                            case _:
                                return Left(hydra.util.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("arbitrary"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.util.Precision, hydra.util.PrecisionArbitrary())), hydra.extract.helpers.decode_unit(cx, input)))), (hydra.core.Name("bits"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.util.Precision, hydra.util.PrecisionBits(t))), (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2)))(cx, input))))))
                return hydra.lib.maybes.maybe(Left(hydra.util.DecodingError(hydra.lib.strings.cat(("no such field ", fname().value, " in union type ", tname().value)))), (lambda f: f(fterm())), hydra.lib.maps.lookup(fname(), variant_map()))
            
            case _:
                return Left(hydra.util.DecodingError("expected union of type hydra.util.Precision"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_util_precision_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
