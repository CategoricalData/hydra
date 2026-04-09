# Note: this is an automatically generated file. Do not edit.

r"""Extraction and validation for hydra.core types."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Nothing, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.errors
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.pairs
import hydra.lib.sets
import hydra.lib.strings
import hydra.show.core
import hydra.show.errors
import hydra.strip

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def bigfloat_value(v: hydra.core.FloatValue) -> Either[hydra.errors.Error, Decimal]:
    r"""Extract a bigfloat value from a FloatValue."""

    match v:
        case hydra.core.FloatValueBigfloat(value=f):
            return Right(f)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("bigfloat", hydra.show.core.float(v)))))))

def float_literal(lit: hydra.core.Literal) -> Either[hydra.errors.Error, hydra.core.FloatValue]:
    r"""Extract a floating-point literal from a Literal value."""

    match lit:
        case hydra.core.LiteralFloat(value=v):
            return Right(v)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("floating-point value", hydra.show.core.literal(lit)))))))

def literal(graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_literal_1(term, v1):
        match v1:
            case hydra.core.TermLiteral(value=lit):
                return Right(lit)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("literal", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_literal_1(term, term)))

def bigfloat(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, Decimal]:
    r"""Extract an arbitrary-precision floating-point value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(l), (lambda f: bigfloat_value(f)))))

def bigint_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract a bigint value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueBigint(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("bigint", hydra.show.core.integer(v)))))))

def integer_literal(lit: hydra.core.Literal) -> Either[hydra.errors.Error, hydra.core.IntegerValue]:
    r"""Extract an integer literal from a Literal value."""

    match lit:
        case hydra.core.LiteralInteger(value=v):
            return Right(v)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("integer value", hydra.show.core.literal(lit)))))))

def bigint(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract an arbitrary-precision integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: bigint_value(i)))))

def binary_literal(v: hydra.core.Literal) -> Either[hydra.errors.Error, bytes]:
    r"""Extract a binary literal from a Literal value."""

    match v:
        case hydra.core.LiteralBinary(value=b):
            return Right(b)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("binary", hydra.show.core.literal(v)))))))

def binary(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, bytes]:
    r"""Extract a binary data value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: binary_literal(l)))

def boolean_literal(v: hydra.core.Literal) -> Either[hydra.errors.Error, bool]:
    r"""Extract a boolean literal from a Literal value."""

    match v:
        case hydra.core.LiteralBoolean(value=b):
            return Right(b)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("boolean", hydra.show.core.literal(v)))))))

def boolean(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, bool]:
    r"""Extract a boolean value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: boolean_literal(l)))

def cases(name: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_cases_1(name, term, v1):
        match v1:
            case hydra.core.EliminationUnion(value=cs):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(cs.type_name.value, name.value), (lambda : Right(cs)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("case statement for type ", name.value), hydra.show.core.term(term)))))))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("case statement", hydra.show.core.term(term)))))))
    def _hoist_hydra_extract_core_cases_2(name, term, v1):
        match v1:
            case hydra.core.FunctionElimination(value=elimination):
                return _hoist_hydra_extract_core_cases_1(name, term, elimination)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("case statement", hydra.show.core.term(term)))))))
    def _hoist_hydra_extract_core_cases_3(name, term, v1):
        match v1:
            case hydra.core.TermFunction(value=function):
                return _hoist_hydra_extract_core_cases_2(name, term, function)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("case statement", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_cases_3(name, term, term)))

def case_field(name: hydra.core.Name, n: str, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Field]:
    r"""Extract a specific case handler from a case statement term."""

    field_name = hydra.core.Name(n)
    return hydra.lib.eithers.bind(cases(name, graph, term), (lambda cs: (matching := hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, field_name.value)), cs.cases), hydra.lib.logic.if_else(hydra.lib.lists.null(matching), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("matching case", "no matching case"))))))), (lambda : Right(hydra.lib.lists.head(matching)))))[1]))

def strip_with_decoding_error(g: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.DecodingError, hydra.core.Term]:
    r"""Strip annotations and dereference variables, returning Either DecodingError Term."""

    return hydra.lib.eithers.bimap((lambda _e: hydra.errors.DecodingError(hydra.show.errors.error(_e))), (lambda x: x), hydra.lexical.strip_and_dereference_term_either(g, term))

def decode_either(left_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], right_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_either_1(g, left_decoder, right_decoder, v1):
        match v1:
            case hydra.core.TermEither(value=e):
                return hydra.lib.eithers.either((lambda lv: hydra.lib.eithers.map((lambda x: Left(x)), left_decoder(g, lv))), (lambda rv: hydra.lib.eithers.map((lambda x: Right(x)), right_decoder(g, rv))), e)

            case _:
                return Left(hydra.errors.DecodingError("expected either value"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_either_1(g, left_decoder, right_decoder, stripped)))

def decode_list(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_list_1(elem_decoder, g, v1):
        match v1:
            case hydra.core.TermList(value=els):
                return hydra.lib.eithers.map_list((lambda v12: elem_decoder(g, v12)), els)

            case _:
                return Left(hydra.errors.DecodingError("expected list"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_list_1(elem_decoder, g, stripped)))

def decode_map(key_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], val_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_map_1(g, key_decoder, val_decoder, v1):
        match v1:
            case hydra.core.TermMap(value=m):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.eithers.map_list((lambda kv: hydra.lib.eithers.bind(key_decoder(g, hydra.lib.pairs.first(kv)), (lambda k: hydra.lib.eithers.map((lambda v: (k, v)), val_decoder(g, hydra.lib.pairs.second(kv)))))), hydra.lib.maps.to_list(m)))

            case _:
                return Left(hydra.errors.DecodingError("expected map"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_map_1(g, key_decoder, val_decoder, stripped)))

def decode_maybe(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_maybe_1(elem_decoder, g, v1):
        match v1:
            case hydra.core.TermMaybe(value=opt):
                return hydra.lib.eithers.map_maybe((lambda v12: elem_decoder(g, v12)), opt)

            case _:
                return Left(hydra.errors.DecodingError("expected optional value"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_maybe_1(elem_decoder, g, stripped)))

def decode_pair(first_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], second_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T1]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_pair_1(first_decoder, g, second_decoder, v1):
        match v1:
            case hydra.core.TermPair(value=p):
                return hydra.lib.eithers.bind(first_decoder(g, hydra.lib.pairs.first(p)), (lambda f: hydra.lib.eithers.map((lambda s: (f, s)), second_decoder(g, hydra.lib.pairs.second(p)))))

            case _:
                return Left(hydra.errors.DecodingError("expected pair"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_pair_1(first_decoder, g, second_decoder, stripped)))

def decode_set(elem_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_set_1(elem_decoder, g, v1):
        match v1:
            case hydra.core.TermSet(value=s):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.sets.from_list(x1)), hydra.lib.eithers.map_list((lambda v12: elem_decoder(g, v12)), hydra.lib.sets.to_list(s)))

            case _:
                return Left(hydra.errors.DecodingError("expected set"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_set_1(elem_decoder, g, stripped)))

def decode_unit(g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_unit_1(v1):
        match v1:
            case hydra.core.TermUnit():
                return Right(None)

            case _:
                return Left(hydra.errors.DecodingError("expected a unit value"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_unit_1(stripped)))

def decode_wrapped(body_decoder: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.errors.DecodingError, T0]], g: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_decode_wrapped_1(body_decoder, g, v1):
        match v1:
            case hydra.core.TermWrap(value=wt):
                return body_decoder(g, wt.body)

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped value"))
    return hydra.lib.eithers.bind(strip_with_decoding_error(g, term), (lambda stripped: _hoist_hydra_extract_core_decode_wrapped_1(body_decoder, g, stripped)))

def either_term(left_fun: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], right_fun: Callable[[hydra.core.Term], Either[hydra.errors.Error, T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_either_term_1(left_fun, right_fun, term, v1):
        match v1:
            case hydra.core.TermEither(value=et):
                return hydra.lib.eithers.either((lambda l: hydra.lib.eithers.map((lambda x: Left(x)), left_fun(l))), (lambda r: hydra.lib.eithers.map((lambda x: Right(x)), right_fun(r))), et)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either value", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_either_term_1(left_fun, right_fun, term, term)))

def either_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.EitherType]:
    r"""Extract the left and right types from an either type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeEither(value=et):
            return Right(et)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("either type", hydra.show.core.type(typ)))))))

def field(fname: hydra.core.Name, mapping: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], graph: hydra.graph.Graph, fields: frozenlist[hydra.core.Field]) -> Either[hydra.errors.Error, T0]:
    r"""Extract a field value from a list of fields."""

    @lru_cache(1)
    def matching_fields() -> frozenlist[hydra.core.Field]:
        return hydra.lib.lists.filter((lambda f: hydra.lib.equality.equal(f.name.value, fname.value)), fields)
    return hydra.lib.logic.if_else(hydra.lib.lists.null(matching_fields()), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("field ", fname.value), "no matching field"))))))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_fields()), 1), (lambda : hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, hydra.lib.lists.head(matching_fields()).term), (lambda stripped: mapping(stripped)))), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("single field", hydra.lib.strings.cat2("multiple fields named ", fname.value)))))))))))

def float32_value(v: hydra.core.FloatValue) -> Either[hydra.errors.Error, float]:
    r"""Extract a float32 value from a FloatValue."""

    match v:
        case hydra.core.FloatValueFloat32(value=f):
            return Right(f)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("float32", hydra.show.core.float(v)))))))

def float32(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, float]:
    r"""Extract a 32-bit floating-point value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(l), (lambda f: float32_value(f)))))

def float64_value(v: hydra.core.FloatValue) -> Either[hydra.errors.Error, float]:
    r"""Extract a float64 value from a FloatValue."""

    match v:
        case hydra.core.FloatValueFloat64(value=f):
            return Right(f)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("float64", hydra.show.core.float(v)))))))

def float64(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, float]:
    r"""Extract a 64-bit floating-point value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(float_literal(l), (lambda f: float64_value(f)))))

def float_value(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.FloatValue]:
    r"""Extract a float value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: float_literal(l)))

def function_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.FunctionType]:
    r"""Extract a function type from a type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeFunction(value=ft):
            return Right(ft)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("function type", hydra.show.core.type(typ)))))))

def injection(expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_injection_1(expected, term, v1):
        match v1:
            case hydra.core.TermUnion(value=injection):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(injection.type_name.value, expected.value), (lambda : Right(injection.field)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("injection of type ", expected.value), injection.type_name.value))))))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("injection", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_injection_1(expected, term, term)))

def int16_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract an int16 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueInt16(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("int16", hydra.show.core.integer(v)))))))

def int16(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 16-bit signed integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: int16_value(i)))))

def int32_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract an int32 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueInt32(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("int32", hydra.show.core.integer(v)))))))

def int32(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 32-bit signed integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: int32_value(i)))))

def int64_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract an int64 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueInt64(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("int64", hydra.show.core.integer(v)))))))

def int64(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 64-bit signed integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: int64_value(i)))))

def int8_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract an int8 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueInt8(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("int8", hydra.show.core.integer(v)))))))

def int8(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract an 8-bit signed integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: int8_value(i)))))

def integer_value(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.IntegerValue]:
    r"""Extract an integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: integer_literal(l)))

def lambda_(graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_lambda_1(term, v1):
        match v1:
            case hydra.core.FunctionLambda(value=l):
                return Right(l)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("lambda", hydra.show.core.term(term)))))))
    def _hoist_hydra_extract_core_lambda_2(term, v1):
        match v1:
            case hydra.core.TermFunction(value=function):
                return _hoist_hydra_extract_core_lambda_1(term, function)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("lambda", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_lambda_2(term, term)))

def lambda_body(graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Extract the body of a lambda term."""

    return hydra.lib.eithers.map((lambda v1: v1.body), lambda_(graph, term))

def let(graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_let_1(term, v1):
        match v1:
            case hydra.core.TermLet(value=lt):
                return Right(lt)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("let term", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_let_1(term, term)))

def let_binding(n: str, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Extract a binding with the given name from a let term."""

    name = hydra.core.Name(n)
    return hydra.lib.eithers.bind(let(graph, term), (lambda let_expr: (matching_bindings := hydra.lib.lists.filter((lambda b: hydra.lib.equality.equal(b.name.value, name.value)), let_expr.bindings), hydra.lib.logic.if_else(hydra.lib.lists.null(matching_bindings), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorNoSuchBinding(hydra.errors.NoSuchBindingError(name))))))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(matching_bindings), 1), (lambda : Right(hydra.lib.lists.head(matching_bindings).term)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorMultipleBindings(hydra.errors.MultipleBindingsError(name)))))))))))[1]))

def list(graph: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_list_1(stripped, v1):
        match v1:
            case hydra.core.TermList(value=l):
                return Right(l)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("list", hydra.show.core.term(stripped)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term), (lambda stripped: _hoist_hydra_extract_core_list_1(stripped, stripped)))

def list_head(graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Term]:
    r"""Extract the first element of a list term."""

    return hydra.lib.eithers.bind(list(graph, term), (lambda l: hydra.lib.logic.if_else(hydra.lib.lists.null(l), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("non-empty list", "empty list"))))))), (lambda : Right(hydra.lib.lists.head(l))))))

def list_of(f: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, frozenlist[T0]]:
    r"""Extract a list of values from a term, mapping a function over each element."""

    return hydra.lib.eithers.bind(list(graph, term), (lambda els: hydra.lib.eithers.map_list(f, els)))

def list_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Extract the element type from a list type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeList(value=t):
            return Right(t)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("list type", hydra.show.core.type(typ)))))))

def map(fk: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], fv: Callable[[hydra.core.Term], Either[hydra.errors.Error, T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    r"""Extract a map of key-value pairs from a term, mapping functions over each key and value."""

    def pair(kv_pair: tuple[hydra.core.Term, hydra.core.Term]) -> Either[hydra.errors.Error, tuple[T0, T1]]:
        @lru_cache(1)
        def kterm() -> hydra.core.Term:
            return hydra.lib.pairs.first(kv_pair)
        @lru_cache(1)
        def vterm() -> hydra.core.Term:
            return hydra.lib.pairs.second(kv_pair)
        return hydra.lib.eithers.bind(fk(kterm()), (lambda kval: hydra.lib.eithers.bind(fv(vterm()), (lambda vval: Right((kval, vval))))))
    def _hoist_pair_body_1(term, v1):
        match v1:
            case hydra.core.TermMap(value=m):
                return hydra.lib.eithers.map((lambda x1: hydra.lib.maps.from_list(x1)), hydra.lib.eithers.map_list((lambda x1: pair(x1)), hydra.lib.maps.to_list(m)))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("map", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_pair_body_1(term, term)))

def map_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.MapType]:
    r"""Extract the key and value types from a map type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeMap(value=mt):
            return Right(mt)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("map type", hydra.show.core.type(typ)))))))

def maybe_term(f: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_maybe_term_1(f, term, v1):
        match v1:
            case hydra.core.TermMaybe(value=mt):
                return hydra.lib.maybes.maybe((lambda : Right(Nothing())), (lambda t: hydra.lib.eithers.map((lambda x1: hydra.lib.maybes.pure(x1)), f(t))), mt)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("maybe value", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_maybe_term_1(f, term, term)))

def maybe_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Extract the base type from an optional type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeMaybe(value=t):
            return Right(t)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("maybe type", hydra.show.core.type(typ)))))))

def n_args(name: hydra.core.Name, n: int, args: frozenlist[T0]) -> Either[hydra.errors.Error, None]:
    r"""Ensure a function has the expected number of arguments."""

    return hydra.lib.logic.if_else(hydra.lib.equality.equal(hydra.lib.lists.length(args), n), (lambda : Right(None)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat((hydra.lib.literals.show_int32(n), " arguments to primitive ", hydra.lib.literals.show_string(name.value))), hydra.lib.literals.show_int32(hydra.lib.lists.length(args))))))))))

def pair(kf: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], vf: Callable[[hydra.core.Term], Either[hydra.errors.Error, T1]], graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_pair_1(kf, term, vf, v1):
        match v1:
            case hydra.core.TermPair(value=p):
                return hydra.lib.eithers.bind(kf(hydra.lib.pairs.first(p)), (lambda k_val: hydra.lib.eithers.bind(vf(hydra.lib.pairs.second(p)), (lambda v_val: Right((k_val, v_val))))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("pair", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_pair_1(kf, term, vf, term)))

def term_record(graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_term_record_1(term, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                return Right(record)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("record", hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_term_record_1(term, term)))

def record(expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term) -> Either[hydra.errors.Error, frozenlist[hydra.core.Field]]:
    r"""Extract a record's fields from a term."""

    return hydra.lib.eithers.bind(term_record(graph, term0), (lambda record: hydra.lib.logic.if_else(hydra.lib.equality.equal(record.type_name, expected), (lambda : Right(record.fields)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("record of type ", expected.value), record.type_name.value))))))))))

def record_type(ename: T0, typ: hydra.core.Type) -> Either[hydra.errors.Error, frozenlist[hydra.core.FieldType]]:
    r"""Extract the field types from a record type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeRecord(value=fields):
            return Right(fields)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("record type", hydra.show.core.type(typ)))))))

def require_field(field_name: str, decoder: Callable[[T0, T1], Either[hydra.errors.DecodingError, T2]], field_map: FrozenDict[hydra.core.Name, T1], g: T0) -> Either[hydra.errors.DecodingError, T2]:
    r"""Require a field from a record's field map and decode it."""

    return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("missing field ", field_name, " in record"))))), (lambda field_term: decoder(g, field_term)), hydra.lib.maps.lookup(hydra.core.Name(field_name), field_map))

def set(graph: hydra.graph.Graph, term: hydra.core.Term):
    def _hoist_hydra_extract_core_set_1(stripped, v1):
        match v1:
            case hydra.core.TermSet(value=s):
                return Right(s)

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("set", hydra.show.core.term(stripped)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term), (lambda stripped: _hoist_hydra_extract_core_set_1(stripped, stripped)))

def set_of(f: Callable[[hydra.core.Term], Either[hydra.errors.Error, T0]], graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, frozenset[T0]]:
    r"""Extract a set of values from a term, mapping a function over each element."""

    return hydra.lib.eithers.bind(set(graph, term), (lambda els: hydra.lib.eithers.map_set(f, els)))

def set_type(typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Extract the element type from a set type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeSet(value=t):
            return Right(t)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("set type", hydra.show.core.type(typ)))))))

def string_literal(v: hydra.core.Literal) -> Either[hydra.errors.Error, str]:
    r"""Extract a string literal from a Literal value."""

    match v:
        case hydra.core.LiteralString(value=s):
            return Right(s)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("string", hydra.show.core.literal(v)))))))

def string(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, str]:
    r"""Extract a string value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: string_literal(l)))

def to_field_map(record: hydra.core.Record) -> FrozenDict[hydra.core.Name, hydra.core.Term]:
    r"""Convert a Record's fields to a Map from Name to Term."""

    return hydra.lib.maps.from_list(hydra.lib.lists.map((lambda f: (f.name, f.term)), record.fields))

def uint16_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract a uint16 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueUint16(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("uint16", hydra.show.core.integer(v)))))))

def uint16(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 16-bit unsigned integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: uint16_value(i)))))

def uint32_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract a uint32 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueUint32(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("uint32", hydra.show.core.integer(v)))))))

def uint32(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 32-bit unsigned integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: uint32_value(i)))))

def uint64_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract a uint64 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueUint64(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("uint64", hydra.show.core.integer(v)))))))

def uint64(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract a 64-bit unsigned integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: uint64_value(i)))))

def uint8_value(v: hydra.core.IntegerValue) -> Either[hydra.errors.Error, int]:
    r"""Extract a uint8 value from an IntegerValue."""

    match v:
        case hydra.core.IntegerValueUint8(value=i):
            return Right(i)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("uint8", hydra.show.core.integer(v)))))))

def uint8(graph: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.Error, int]:
    r"""Extract an 8-bit unsigned integer value from a term."""

    return hydra.lib.eithers.bind(literal(graph, t), (lambda l: hydra.lib.eithers.bind(integer_literal(l), (lambda i: uint8_value(i)))))

def union_type(ename: T0, typ: hydra.core.Type) -> Either[hydra.errors.Error, frozenlist[hydra.core.FieldType]]:
    r"""Extract the field types from a union type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeUnion(value=fields):
            return Right(fields)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("union type", hydra.show.core.type(typ)))))))

def unit(term: hydra.core.Term) -> Either[hydra.errors.Error, None]:
    r"""Extract a unit value from a term."""

    match term:
        case hydra.core.TermUnit():
            return Right(None)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("unit", hydra.show.core.term(term)))))))

def unit_variant(tname: hydra.core.Name, graph: hydra.graph.Graph, term: hydra.core.Term) -> Either[hydra.errors.Error, hydra.core.Name]:
    r"""Extract a unit variant (a variant with an empty record value) from a union term."""

    return hydra.lib.eithers.bind(injection(tname, graph, term), (lambda field: hydra.lib.eithers.bind(unit(field.term), (lambda ignored: Right(field.name)))))

def wrap(expected: hydra.core.Name, graph: hydra.graph.Graph, term0: hydra.core.Term):
    def _hoist_hydra_extract_core_wrap_1(expected, term, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.logic.if_else(hydra.lib.equality.equal(wrapped_term.type_name.value, expected.value), (lambda : Right(wrapped_term.body)), (lambda : Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2("wrapper of type ", expected.value), wrapped_term.type_name.value))))))))

            case _:
                return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError(hydra.lib.strings.cat2(hydra.lib.strings.cat2("wrap(", expected.value), ")"), hydra.show.core.term(term)))))))
    return hydra.lib.eithers.bind(hydra.lexical.strip_and_dereference_term(graph, term0), (lambda term: _hoist_hydra_extract_core_wrap_1(expected, term, term)))

def wrapped_type(ename: T0, typ: hydra.core.Type) -> Either[hydra.errors.Error, hydra.core.Type]:
    r"""Extract the wrapped type from a wrapper type."""

    @lru_cache(1)
    def stripped() -> hydra.core.Type:
        return hydra.strip.deannotate_type(typ)
    match stripped():
        case hydra.core.TypeWrap(value=inner_type):
            return Right(inner_type)

        case _:
            return Left(cast(hydra.errors.Error, hydra.errors.ErrorExtraction(cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(hydra.errors.UnexpectedShapeError("wrapped type", hydra.show.core.type(typ)))))))
