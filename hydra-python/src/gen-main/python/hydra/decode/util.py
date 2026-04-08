# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.util."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Right
from typing import cast
import hydra.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings
import hydra.util

def case_convention(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_util_case_convention_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.util.CaseConvention]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("camel"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.CAMEL), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("pascal"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.PASCAL), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("lowerSnake"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.LOWER_SNAKE), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("upperSnake"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.CaseConvention.UPPER_SNAKE), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_util_case_convention_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def comparison(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_util_comparison_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.util.Comparison]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("lessThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.LESS_THAN), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("equalTo"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.EQUAL_TO), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("greaterThan"), (lambda input: hydra.lib.eithers.map((lambda t: hydra.util.Comparison.GREATER_THAN), hydra.extract.core.decode_unit(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_util_comparison_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def precision(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_util_precision_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map():
                    def _hoist_variant_map_1(v12):
                        match v12:
                            case hydra.core.IntegerValueInt32(value=i):
                                return Right(i)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 value"))
                    def _hoist_variant_map_2(v12):
                        match v12:
                            case hydra.core.LiteralInteger(value=_match_value):
                                return _hoist_variant_map_1(_match_value)

                            case _:
                                return Left(hydra.errors.DecodingError("expected int32 literal"))
                    def _hoist_variant_map_3(v12):
                        match v12:
                            case hydra.core.TermLiteral(value=v):
                                return _hoist_variant_map_2(v)

                            case _:
                                return Left(hydra.errors.DecodingError("expected literal"))
                    return hydra.lib.maps.from_list(((hydra.core.Name("arbitrary"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.util.Precision, hydra.util.PrecisionArbitrary())), hydra.extract.core.decode_unit(cx, input)))), (hydra.core.Name("bits"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.util.Precision, hydra.util.PrecisionBits(t))), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_variant_map_3(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, input)))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_util_precision_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
