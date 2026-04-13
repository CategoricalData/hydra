# Note: this is an automatically generated file. Do not edit.

r"""Type preparation functions for Scala code generation."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.lib.literals
import hydra.lib.pairs
import hydra.lib.sets
import hydra.rewriting

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")

def same(x: T0) -> tuple[T0, tuple[Callable[[T1], T1], frozenset[T2]]]:
    r"""Return a type unchanged with identity transform and no messages."""

    return (x, ((lambda y: y), hydra.lib.sets.empty()))

def prepare_float_type(ft: hydra.core.FloatType):
    def _hoist_hydra_ext_scala_prepare_prepare_float_type_1(v, v1):
        match v1:
            case hydra.core.FloatValueBigfloat(value=d):
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(d)))

            case _:
                return v
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            return (hydra.core.FloatType.FLOAT64, ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_float_type_1(v, v)), hydra.lib.sets.from_list(("replace arbitrary-precision floating-point numbers with 64-bit floating-point numbers (doubles)",))))

        case _:
            return same(ft)

def prepare_integer_type(it: hydra.core.IntegerType):
    def _hoist_hydra_ext_scala_prepare_prepare_integer_type_1(v, v1):
        match v1:
            case hydra.core.IntegerValueBigint(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(i)))

            case _:
                return v
    def _hoist_hydra_ext_scala_prepare_prepare_integer_type_2(v, v1):
        match v1:
            case hydra.core.IntegerValueUint8(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(hydra.lib.literals.bigint_to_int8(hydra.lib.literals.uint8_to_bigint(i))))

            case _:
                return v
    def _hoist_hydra_ext_scala_prepare_prepare_integer_type_3(v, v1):
        match v1:
            case hydra.core.IntegerValueUint32(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(hydra.lib.literals.uint32_to_bigint(i))))

            case _:
                return v
    def _hoist_hydra_ext_scala_prepare_prepare_integer_type_4(v, v1):
        match v1:
            case hydra.core.IntegerValueUint64(value=i):
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(hydra.lib.literals.uint64_to_bigint(i))))

            case _:
                return v
    match it:
        case hydra.core.IntegerType.BIGINT:
            return (hydra.core.IntegerType.INT64, ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_integer_type_1(v, v)), hydra.lib.sets.from_list(("replace arbitrary-precision integers with 64-bit integers",))))

        case hydra.core.IntegerType.UINT8:
            return (hydra.core.IntegerType.INT8, ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_integer_type_2(v, v)), hydra.lib.sets.from_list(("replace unsigned 8-bit integers with signed 8-bit integers",))))

        case hydra.core.IntegerType.UINT32:
            return (hydra.core.IntegerType.INT32, ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_integer_type_3(v, v)), hydra.lib.sets.from_list(("replace unsigned 32-bit integers with signed 32-bit integers",))))

        case hydra.core.IntegerType.UINT64:
            return (hydra.core.IntegerType.INT64, ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_integer_type_4(v, v)), hydra.lib.sets.from_list(("replace unsigned 64-bit integers with signed 64-bit integers",))))

        case _:
            return same(it)

def prepare_literal_type(at: hydra.core.LiteralType):
    def _hoist_hydra_ext_scala_prepare_prepare_literal_type_1(v, v1):
        match v1:
            case hydra.core.LiteralBinary(value=b):
                return cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b)))

            case _:
                return v
    match at:
        case hydra.core.LiteralTypeBinary():
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()), ((lambda v: _hoist_hydra_ext_scala_prepare_prepare_literal_type_1(v, v)), hydra.lib.sets.from_list(("replace binary strings with character strings",))))

        case hydra.core.LiteralTypeFloat(value=ft):
            @lru_cache(1)
            def result() -> tuple[hydra.core.FloatType, tuple[Callable[[hydra.core.FloatValue], hydra.core.FloatValue], frozenset[str]]]:
                return prepare_float_type(ft)
            @lru_cache(1)
            def rtyp() -> hydra.core.FloatType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.FloatValue], hydra.core.FloatValue]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.LiteralFloat(value=fv):
                        return cast(hydra.core.Literal, hydra.core.LiteralFloat(rep(fv)))

                    case _:
                        return v
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case hydra.core.LiteralTypeInteger(value=it):
            @lru_cache(1)
            def result() -> tuple[hydra.core.IntegerType, tuple[Callable[[hydra.core.IntegerValue], hydra.core.IntegerValue], frozenset[str]]]:
                return prepare_integer_type(it)
            @lru_cache(1)
            def rtyp() -> hydra.core.IntegerType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.IntegerValue], hydra.core.IntegerValue]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.LiteralInteger(value=iv):
                        return cast(hydra.core.Literal, hydra.core.LiteralInteger(rep(iv)))

                    case _:
                        return v
            return (cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case _:
            return same(at)

def prepare_type(cx: T0, typ: hydra.core.Type):
    r"""Prepare a type for Scala code generation, substituting unsupported types."""

    match hydra.rewriting.deannotate_type(typ):
        case hydra.core.TypeLiteral(value=at):
            @lru_cache(1)
            def result() -> tuple[hydra.core.LiteralType, tuple[Callable[[hydra.core.Literal], hydra.core.Literal], frozenset[str]]]:
                return prepare_literal_type(at)
            @lru_cache(1)
            def rtyp() -> hydra.core.LiteralType:
                return hydra.lib.pairs.first(result())
            @lru_cache(1)
            def rep() -> Callable[[hydra.core.Literal], hydra.core.Literal]:
                return hydra.lib.pairs.first(hydra.lib.pairs.second(result()))
            @lru_cache(1)
            def msgs() -> frozenset[str]:
                return hydra.lib.pairs.second(hydra.lib.pairs.second(result()))
            def _hoist_result_body_1(v, v1):
                match v1:
                    case hydra.core.TermLiteral(value=av):
                        return cast(hydra.core.Term, hydra.core.TermLiteral(rep(av)))

                    case _:
                        return v
            return (cast(hydra.core.Type, hydra.core.TypeLiteral(rtyp())), ((lambda v: _hoist_result_body_1(v, v)), msgs()))

        case _:
            return same(typ)
