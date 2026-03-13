# Note: this is an automatically generated file. Do not edit.

r"""Adapter framework for literal types and terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right, frozenlist
from typing import TypeVar, cast
import hydra.adapt.utils
import hydra.coders
import hydra.compute
import hydra.context
import hydra.core
import hydra.error
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.equality
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.sets
import hydra.lib.strings
import hydra.reflect
import hydra.show.core
import hydra.util
import hydra.variants

T0 = TypeVar("T0")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
T3 = TypeVar("T3")

def compare_precision(p1: hydra.util.Precision, p2: hydra.util.Precision):
    def _hoist_hydra_adapt_literals_compare_precision_1(v1):
        match v1:
            case hydra.util.PrecisionArbitrary():
                return hydra.util.Comparison.EQUAL_TO
            
            case hydra.util.PrecisionBits():
                return hydra.util.Comparison.GREATER_THAN
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def _hoist_hydra_adapt_literals_compare_precision_2(b1, v1):
        match v1:
            case hydra.util.PrecisionArbitrary():
                return hydra.util.Comparison.LESS_THAN
            
            case hydra.util.PrecisionBits(value=b2):
                return hydra.lib.logic.if_else(hydra.lib.equality.lt(b1, b2), (lambda : hydra.util.Comparison.LESS_THAN), (lambda : hydra.util.Comparison.GREATER_THAN))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    match p1:
        case hydra.util.PrecisionArbitrary():
            return _hoist_hydra_adapt_literals_compare_precision_1(p2)
        
        case hydra.util.PrecisionBits(value=b1):
            return _hoist_hydra_adapt_literals_compare_precision_2(b1, p2)
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def convert_float_value(target: hydra.core.FloatType, fv: hydra.core.FloatValue) -> hydra.core.FloatValue:
    r"""Convert a float value to a different float type."""
    
    def decoder(fv2: hydra.core.FloatValue) -> Decimal:
        match fv2:
            case hydra.core.FloatValueBigfloat(value=d):
                return d
            
            case hydra.core.FloatValueFloat32(value=f):
                return hydra.lib.literals.float32_to_bigfloat(f)
            
            case hydra.core.FloatValueFloat64(value=d2):
                return hydra.lib.literals.float64_to_bigfloat(d2)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encoder(d: Decimal) -> hydra.core.FloatValue:
        match target:
            case hydra.core.FloatType.BIGFLOAT:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(d))
            
            case hydra.core.FloatType.FLOAT32:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(hydra.lib.literals.bigfloat_to_float32(d)))
            
            case hydra.core.FloatType.FLOAT64:
                return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(d)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return encoder(decoder(fv))

def convert_integer_value(target: hydra.core.IntegerType, iv: hydra.core.IntegerValue) -> hydra.core.IntegerValue:
    r"""Convert an integer value to a different integer type."""
    
    def decoder(iv2: hydra.core.IntegerValue) -> int:
        match iv2:
            case hydra.core.IntegerValueBigint(value=v):
                return v
            
            case hydra.core.IntegerValueInt8(value=v2):
                return hydra.lib.literals.int8_to_bigint(v2)
            
            case hydra.core.IntegerValueInt16(value=v3):
                return hydra.lib.literals.int16_to_bigint(v3)
            
            case hydra.core.IntegerValueInt32(value=v4):
                return hydra.lib.literals.int32_to_bigint(v4)
            
            case hydra.core.IntegerValueInt64(value=v5):
                return hydra.lib.literals.int64_to_bigint(v5)
            
            case hydra.core.IntegerValueUint8(value=v6):
                return hydra.lib.literals.uint8_to_bigint(v6)
            
            case hydra.core.IntegerValueUint16(value=v7):
                return hydra.lib.literals.uint16_to_bigint(v7)
            
            case hydra.core.IntegerValueUint32(value=v8):
                return hydra.lib.literals.uint32_to_bigint(v8)
            
            case hydra.core.IntegerValueUint64(value=v9):
                return hydra.lib.literals.uint64_to_bigint(v9)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def encoder(d: int) -> hydra.core.IntegerValue:
        match target:
            case hydra.core.IntegerType.BIGINT:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(d))
            
            case hydra.core.IntegerType.INT8:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(hydra.lib.literals.bigint_to_int8(d)))
            
            case hydra.core.IntegerType.INT16:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(hydra.lib.literals.bigint_to_int16(d)))
            
            case hydra.core.IntegerType.INT32:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(d)))
            
            case hydra.core.IntegerType.INT64:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(d)))
            
            case hydra.core.IntegerType.UINT8:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(hydra.lib.literals.bigint_to_uint8(d)))
            
            case hydra.core.IntegerType.UINT16:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(hydra.lib.literals.bigint_to_uint16(d)))
            
            case hydra.core.IntegerType.UINT32:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(hydra.lib.literals.bigint_to_uint32(d)))
            
            case hydra.core.IntegerType.UINT64:
                return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(hydra.lib.literals.bigint_to_uint64(d)))
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return encoder(decoder(iv))

def disclaimer(lossy: bool, source: str, target: str) -> str:
    r"""Generate a disclaimer message for type conversions."""
    
    return hydra.lib.strings.cat(("replace ", source, " with ", target, hydra.lib.logic.if_else(lossy, (lambda : " (lossy)"), (lambda : ""))))

def float_adapter(cx: hydra.coders.AdapterContext, ft: hydra.core.FloatType) -> Either[str, hydra.compute.Adapter[hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]:
    r"""Create an adapter for float types."""
    
    def make_adapter(source: hydra.core.FloatType, target: hydra.core.FloatType) -> Either[T0, hydra.compute.Adapter[hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]:
        @lru_cache(1)
        def lossy() -> bool:
            return hydra.lib.equality.equal(compare_precision(hydra.reflect.float_type_precision(source), hydra.reflect.float_type_precision(target)), hydra.util.Comparison.GREATER_THAN)
        @lru_cache(1)
        def step() -> hydra.compute.Coder[hydra.core.FloatValue, hydra.core.FloatValue]:
            return hydra.compute.Coder((lambda _cx, fv: Right(convert_float_value(target, fv))), (lambda _cx, fv: Right(convert_float_value(source, fv))))
        return Right(hydra.compute.Adapter(lossy(), source, target, step()))
    def alt_types(t: hydra.core.FloatType) -> frozenlist[hydra.core.FloatType]:
        match t:
            case hydra.core.FloatType.BIGFLOAT:
                return (hydra.core.FloatType.FLOAT64, hydra.core.FloatType.FLOAT32)
            
            case hydra.core.FloatType.FLOAT32:
                return (hydra.core.FloatType.FLOAT64, hydra.core.FloatType.BIGFLOAT)
            
            case hydra.core.FloatType.FLOAT64:
                return (hydra.core.FloatType.BIGFLOAT, hydra.core.FloatType.FLOAT32)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def alts(t: hydra.core.FloatType) -> Either[T0, frozenlist[hydra.compute.Adapter[hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]]:
        return hydra.lib.eithers.map_list((lambda v1: make_adapter(t, v1)), alt_types(t))
    def supported(v1: hydra.core.FloatType) -> bool:
        return hydra.adapt.utils.float_type_is_supported(cx.language.constraints, v1)
    return hydra.adapt.utils.choose_adapter((lambda x1: alts(x1)), (lambda x1: supported(x1)), (lambda x1: hydra.show.core.float_type(x1)), (lambda x1: hydra.show.core.float_type(x1)), ft)

def integer_adapter(cx: hydra.coders.AdapterContext, it: hydra.core.IntegerType) -> Either[str, hydra.compute.Adapter[hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]:
    r"""Create an adapter for integer types."""
    
    def interleave(xs: frozenlist[T0], ys: frozenlist[T0]) -> frozenlist[T0]:
        return hydra.lib.lists.concat(hydra.lib.lists.transpose((xs, ys)))
    @lru_cache(1)
    def signed_ordered() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.reflect.integer_type_is_signed(v), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.reflect.integer_type_precision(v), cast(hydra.util.Precision, hydra.util.PrecisionArbitrary()))))), hydra.reflect.integer_types)
    @lru_cache(1)
    def unsigned_ordered() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.reflect.integer_type_is_signed(v)), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.reflect.integer_type_precision(v), cast(hydra.util.Precision, hydra.util.PrecisionArbitrary()))))), hydra.reflect.integer_types)
    @lru_cache(1)
    def signed_pref() -> frozenlist[hydra.core.IntegerType]:
        return interleave(signed_ordered(), unsigned_ordered())
    @lru_cache(1)
    def unsigned_pref() -> frozenlist[hydra.core.IntegerType]:
        return interleave(unsigned_ordered(), signed_ordered())
    @lru_cache(1)
    def signed_non_pref() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.reverse(unsigned_pref())
    @lru_cache(1)
    def unsigned_non_pref() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.reverse(signed_pref())
    def signed(i: int) -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.concat((hydra.lib.lists.drop(hydra.lib.math.mul(i, 2), signed_pref()), (hydra.core.IntegerType.BIGINT,), hydra.lib.lists.drop(hydra.lib.math.add(hydra.lib.math.sub(8, hydra.lib.math.mul(i, 2)), 1), signed_non_pref())))
    def unsigned(i: int) -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.concat((hydra.lib.lists.drop(hydra.lib.math.mul(i, 2), unsigned_pref()), (hydra.core.IntegerType.BIGINT,), hydra.lib.lists.drop(hydra.lib.math.add(hydra.lib.math.sub(8, hydra.lib.math.mul(i, 2)), 1), unsigned_non_pref())))
    def make_adapter(source: hydra.core.IntegerType, target: hydra.core.IntegerType) -> Either[T0, hydra.compute.Adapter[hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]:
        @lru_cache(1)
        def lossy() -> bool:
            return hydra.lib.logic.not_(hydra.lib.equality.equal(compare_precision(hydra.reflect.integer_type_precision(source), hydra.reflect.integer_type_precision(target)), hydra.util.Comparison.LESS_THAN))
        @lru_cache(1)
        def step() -> hydra.compute.Coder[hydra.core.IntegerValue, hydra.core.IntegerValue]:
            return hydra.compute.Coder((lambda _cx, iv: Right(convert_integer_value(target, iv))), (lambda _cx, iv: Right(convert_integer_value(source, iv))))
        return Right(hydra.compute.Adapter(lossy(), source, target, step()))
    def alt_types(t: hydra.core.IntegerType) -> frozenlist[hydra.core.IntegerType]:
        match t:
            case hydra.core.IntegerType.BIGINT:
                return hydra.lib.lists.reverse(unsigned_pref())
            
            case hydra.core.IntegerType.INT8:
                return signed(1)
            
            case hydra.core.IntegerType.INT16:
                return signed(2)
            
            case hydra.core.IntegerType.INT32:
                return signed(3)
            
            case hydra.core.IntegerType.INT64:
                return signed(4)
            
            case hydra.core.IntegerType.UINT8:
                return unsigned(1)
            
            case hydra.core.IntegerType.UINT16:
                return unsigned(2)
            
            case hydra.core.IntegerType.UINT32:
                return unsigned(3)
            
            case hydra.core.IntegerType.UINT64:
                return unsigned(4)
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def alts(t: hydra.core.IntegerType) -> Either[T0, frozenlist[hydra.compute.Adapter[hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]]:
        return hydra.lib.eithers.map_list((lambda v1: make_adapter(t, v1)), alt_types(t))
    def supported(v1: hydra.core.IntegerType) -> bool:
        return hydra.adapt.utils.integer_type_is_supported(cx.language.constraints, v1)
    return hydra.adapt.utils.choose_adapter((lambda x1: alts(x1)), (lambda x1: supported(x1)), (lambda x1: hydra.show.core.integer_type(x1)), (lambda x1: hydra.show.core.integer_type(x1)), it)

def literal_adapter(cx: hydra.coders.AdapterContext, lt: hydra.core.LiteralType) -> Either[str, hydra.compute.Adapter[hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]:
    r"""Create an adapter for literal types."""
    
    def for_binary(t: T0) -> Either[T1, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def match_binary(_cx: T2, lit: hydra.core.Literal) -> Either[T3, hydra.core.Literal]:
            match lit:
                case hydra.core.LiteralBinary(value=b):
                    return Right(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        def match_string(_cx: T2, lit: hydra.core.Literal) -> Either[T3, hydra.core.Literal]:
            match lit:
                case hydra.core.LiteralString(value=s):
                    return Right(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        step = hydra.compute.Coder((lambda x1, x2: match_binary(x1, x2)), (lambda x1, x2: match_string(x1, x2)))
        return Right((hydra.compute.Adapter(False, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()), step),))
    def for_boolean(t: T0) -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def match_boolean(step_: hydra.compute.Coder[hydra.core.IntegerValue, hydra.core.IntegerValue], cx2: hydra.context.Context, lit: hydra.core.Literal):
            def _hoist_match_boolean_1(cx2, step_, v1):
                match v1:
                    case hydra.core.LiteralBoolean(value=bv):
                        return hydra.lib.eithers.bind(step_.encode(cx2, cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(hydra.lib.logic.if_else(bv, (lambda : 1), (lambda : 0))))), (lambda iv: Right(cast(hydra.core.Literal, hydra.core.LiteralInteger(iv)))))
                    
                    case _:
                        raise TypeError("Unsupported Literal")
            return _hoist_match_boolean_1(cx2, step_, lit)
        def match_integer(step_: hydra.compute.Coder[hydra.core.IntegerValue, hydra.core.IntegerValue], cx2: hydra.context.Context, lit: hydra.core.Literal):
            def for_value(val: hydra.core.IntegerValue):
                def _hoist_for_value_1(v1):
                    match v1:
                        case hydra.core.IntegerValueUint8(value=v):
                            return cast(hydra.core.Literal, hydra.core.LiteralBoolean(hydra.lib.equality.equal(v, 1)))
                        
                        case _:
                            raise TypeError("Unsupported IntegerValue")
                return _hoist_for_value_1(val)
            def _hoist_body_1(v1):
                match v1:
                    case hydra.core.LiteralInteger(value=iv):
                        return hydra.lib.eithers.bind(step_.decode(cx2, iv), (lambda val: Right(for_value(val))))
                    
                    case _:
                        raise TypeError("Unsupported Literal")
            return _hoist_body_1(lit)
        constraints = cx.language.constraints
        @lru_cache(1)
        def has_integers() -> bool:
            return hydra.lib.logic.not_(hydra.lib.sets.null(constraints.integer_types))
        @lru_cache(1)
        def has_strings() -> bool:
            return hydra.lib.sets.member(hydra.variants.LiteralVariant.STRING, constraints.literal_variants)
        @lru_cache(1)
        def with_integers() -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
            def with_adapter(adapter: hydra.compute.Adapter[T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]) -> Either[T2, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
                @lru_cache(1)
                def step_() -> hydra.compute.Coder[hydra.core.IntegerValue, hydra.core.IntegerValue]:
                    return adapter.coder
                @lru_cache(1)
                def step() -> hydra.compute.Coder[hydra.core.Literal, hydra.core.Literal]:
                    return hydra.compute.Coder((lambda v1, v2: match_boolean(step_(), v1, v2)), (lambda v1, v2: match_integer(step_(), v1, v2)))
                return Right((hydra.compute.Adapter(False, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(adapter.target)), step()),))
            return hydra.lib.eithers.bind(integer_adapter(cx, hydra.core.IntegerType.UINT8), (lambda adapter: with_adapter(adapter)))
        @lru_cache(1)
        def with_strings() -> frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]:
            def encode(cx2: hydra.context.Context, lit: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Literal]:
                return hydra.lib.eithers.bind(hydra.extract.core.boolean_literal(cx2, lit), (lambda b: Right(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.logic.if_else(b, (lambda : "true"), (lambda : "false")))))))
            def decode(cx2: hydra.context.Context, lit: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Literal]:
                return hydra.lib.eithers.bind(hydra.extract.core.string_literal(cx2, lit), (lambda s: hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "true"), (lambda : Right(cast(hydra.core.Literal, hydra.core.LiteralBoolean(True)))), (lambda : hydra.lib.logic.if_else(hydra.lib.equality.equal(s, "false"), (lambda : Right(cast(hydra.core.Literal, hydra.core.LiteralBoolean(False)))), (lambda : Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("expected boolean literal, found ", s)))), cx2))))))))
            return (hydra.compute.Adapter(False, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()), hydra.compute.Coder((lambda x1, x2: encode(x1, x2)), (lambda x1, x2: decode(x1, x2)))),)
        return hydra.lib.logic.if_else(has_integers(), (lambda : with_integers()), (lambda : hydra.lib.logic.if_else(has_strings(), (lambda : Right(with_strings())), (lambda : Left("no alternatives available for boolean encoding")))))
    def for_float(t: T0, ft: hydra.core.FloatType) -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        @lru_cache(1)
        def with_floats() -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
            def adapt(adapter: hydra.compute.Adapter[T1, T2, hydra.core.FloatValue, hydra.core.FloatValue], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, l: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Literal]:
                match l:
                    case hydra.core.LiteralFloat(value=fv):
                        return hydra.lib.eithers.map((lambda x: cast(hydra.core.Literal, hydra.core.LiteralFloat(x))), hydra.adapt.utils.encode_decode(dir, adapter.coder, cx2, fv))
                    
                    case _:
                        return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("expected floating-point literal, found ", hydra.show.core.literal(l))))), cx2))
            return hydra.lib.eithers.bind(float_adapter(cx, ft), (lambda adapter: (step := hydra.adapt.utils.bidirectional((lambda v1, v2, v3: adapt(adapter, v1, v2, v3))), Right((hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(adapter.target)), step),)))[1]))
        constraints = cx.language.constraints
        @lru_cache(1)
        def has_floats() -> bool:
            return hydra.lib.logic.not_(hydra.lib.sets.null(constraints.float_types))
        return hydra.lib.logic.if_else(has_floats(), (lambda : with_floats()), (lambda : Left("no float types available")))
    def for_integer(t: T0, it: hydra.core.IntegerType) -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        @lru_cache(1)
        def with_integers() -> Either[str, frozenlist[hydra.compute.Adapter[T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
            def adapt(adapter: hydra.compute.Adapter[T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue], dir: hydra.coders.CoderDirection, cx2: hydra.context.Context, lit: hydra.core.Literal) -> Either[hydra.context.InContext[hydra.error.Error], hydra.core.Literal]:
                match lit:
                    case hydra.core.LiteralInteger(value=iv):
                        return hydra.lib.eithers.map((lambda x: cast(hydra.core.Literal, hydra.core.LiteralInteger(x))), hydra.adapt.utils.encode_decode(dir, adapter.coder, cx2, iv))
                    
                    case _:
                        return Left(hydra.context.InContext(cast(hydra.error.Error, hydra.error.ErrorOther(hydra.error.OtherError(hydra.lib.strings.cat2("expected integer literal, found ", hydra.show.core.literal(lit))))), cx2))
            return hydra.lib.eithers.bind(integer_adapter(cx, it), (lambda adapter: (step := hydra.adapt.utils.bidirectional((lambda v1, v2, v3: adapt(adapter, v1, v2, v3))), Right((hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(adapter.target)), step),)))[1]))
        constraints = cx.language.constraints
        @lru_cache(1)
        def has_integers() -> bool:
            return hydra.lib.logic.not_(hydra.lib.sets.null(constraints.integer_types))
        return hydra.lib.logic.if_else(has_integers(), (lambda : with_integers()), (lambda : Left("no integer types available")))
    def alts(t: hydra.core.LiteralType) -> Either[str, frozenlist[hydra.compute.Adapter[hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        match t:
            case hydra.core.LiteralTypeBinary():
                return for_binary(t)
            
            case hydra.core.LiteralTypeBoolean():
                return for_boolean(t)
            
            case hydra.core.LiteralTypeFloat(value=ft):
                return for_float(t, ft)
            
            case hydra.core.LiteralTypeInteger(value=it):
                return for_integer(t, it)
            
            case hydra.core.LiteralTypeString():
                return Left("no substitute for the literal string type")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    def supported(v1: hydra.core.LiteralType) -> bool:
        return hydra.adapt.utils.literal_type_is_supported(cx.language.constraints, v1)
    return hydra.adapt.utils.choose_adapter((lambda x1: alts(x1)), (lambda x1: supported(x1)), (lambda x1: hydra.show.core.literal_type(x1)), (lambda x1: hydra.show.core.literal_type(x1)), lt)
