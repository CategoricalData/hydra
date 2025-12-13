# Note: this is an automatically generated file. Do not edit.

r"""Adapter framework for literal types and terms."""

from __future__ import annotations
from collections.abc import Callable
from decimal import Decimal
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.adapt.utils
import hydra.coders
import hydra.compute
import hydra.core
import hydra.extract.core
import hydra.lib.equality
import hydra.lib.flows
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.logic
import hydra.lib.math
import hydra.lib.sets
import hydra.lib.strings
import hydra.monads
import hydra.reflect
import hydra.show.core
import hydra.util
import hydra.variants

def compare_precision(p1: hydra.util.Precision, p2: hydra.util.Precision) -> hydra.util.Comparison:
    r"""Compare two precision values."""
    
    match p1:
        case hydra.util.PrecisionArbitrary():
            match p2:
                case hydra.util.PrecisionArbitrary():
                    return hydra.util.Comparison.EQUAL_TO
                
                case hydra.util.PrecisionBits():
                    return hydra.util.Comparison.GREATER_THAN
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        
        case hydra.util.PrecisionBits(value=b1):
            match p2:
                case hydra.util.PrecisionArbitrary():
                    return hydra.util.Comparison.LESS_THAN
                
                case hydra.util.PrecisionBits(value=b2):
                    return hydra.lib.logic.if_else(hydra.lib.equality.lt(b1, b2), (lambda : hydra.util.Comparison.LESS_THAN), (lambda : hydra.util.Comparison.GREATER_THAN))
                
                case _:
                    raise AssertionError("Unreachable: all variants handled")
        
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

def float_adapter[T0, T1](ft: hydra.core.FloatType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[T0, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]:
    def make_adapter[T2, T3, T4](source: hydra.core.FloatType, target: hydra.core.FloatType) -> hydra.compute.Flow[T2, hydra.compute.Adapter[T3, T4, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]:
        def lossy() -> bool:
            return hydra.lib.equality.equal(compare_precision(hydra.reflect.float_type_precision(source), hydra.reflect.float_type_precision(target)), hydra.util.Comparison.GREATER_THAN)
        def step[T5, T6]() -> hydra.compute.Coder[T5, T6, hydra.core.FloatValue, hydra.core.FloatValue]:
            return cast(hydra.compute.Coder[T5, T6, hydra.core.FloatValue, hydra.core.FloatValue], hydra.compute.Coder((lambda fv: hydra.lib.flows.pure(convert_float_value(target, fv))), (lambda fv: hydra.lib.flows.pure(convert_float_value(source, fv)))))
        def msg() -> str:
            return disclaimer(lossy(), hydra.show.core.float_type(source), hydra.show.core.float_type(target))
        return hydra.monads.warn(msg(), hydra.lib.flows.pure(cast(hydra.compute.Adapter[T3, T4, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue], hydra.compute.Adapter(lossy(), source, target, cast(hydra.compute.Coder[T3, T4, hydra.core.FloatValue, hydra.core.FloatValue], step())))))
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
    def alts[T2, T3, T4](t: hydra.core.FloatType) -> hydra.compute.Flow[T2, frozenlist[hydra.compute.Adapter[T3, T4, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]]:
        return hydra.lib.flows.map_list((lambda v1: make_adapter(t, v1)), alt_types(t))
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (supported := (lambda v1: hydra.adapt.utils.float_type_is_supported(cx.language.constraints, v1)), hydra.adapt.utils.choose_adapter(cast(Callable[[hydra.core.FloatType], hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T0, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue]]]], (lambda x1: alts(x1))), supported, hydra.show.core.float_type, hydra.show.core.float_type, ft))[1]))

def integer_adapter[T0, T1](it: hydra.core.IntegerType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[T0, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]:
    def interleave[T2](xs: frozenlist[T2], ys: frozenlist[T2]) -> frozenlist[T2]:
        return hydra.lib.lists.concat(hydra.lib.lists.transpose((xs, ys)))
    def signed_ordered() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.reflect.integer_type_is_signed(v), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.reflect.integer_type_precision(v), cast(hydra.util.Precision, hydra.util.PrecisionArbitrary()))))), hydra.reflect.integer_types)
    def unsigned_ordered() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.filter((lambda v: hydra.lib.logic.and_(hydra.lib.logic.not_(hydra.reflect.integer_type_is_signed(v)), hydra.lib.logic.not_(hydra.lib.equality.equal(hydra.reflect.integer_type_precision(v), cast(hydra.util.Precision, hydra.util.PrecisionArbitrary()))))), hydra.reflect.integer_types)
    def signed_pref() -> frozenlist[hydra.core.IntegerType]:
        return interleave(signed_ordered(), unsigned_ordered())
    def unsigned_pref() -> frozenlist[hydra.core.IntegerType]:
        return interleave(unsigned_ordered(), signed_ordered())
    def signed_non_pref() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.reverse(unsigned_pref())
    def unsigned_non_pref() -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.reverse(signed_pref())
    def signed(i: int) -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.concat((hydra.lib.lists.drop(hydra.lib.math.mul(i, 2), signed_pref()), (hydra.core.IntegerType.BIGINT,), hydra.lib.lists.drop(hydra.lib.math.add(hydra.lib.math.sub(8, hydra.lib.math.mul(i, 2)), 1), signed_non_pref())))
    def unsigned(i: int) -> frozenlist[hydra.core.IntegerType]:
        return hydra.lib.lists.concat((hydra.lib.lists.drop(hydra.lib.math.mul(i, 2), unsigned_pref()), (hydra.core.IntegerType.BIGINT,), hydra.lib.lists.drop(hydra.lib.math.add(hydra.lib.math.sub(8, hydra.lib.math.mul(i, 2)), 1), unsigned_non_pref())))
    def make_adapter[T2, T3, T4](source: hydra.core.IntegerType, target: hydra.core.IntegerType) -> hydra.compute.Flow[T2, hydra.compute.Adapter[T3, T4, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]:
        def lossy() -> bool:
            return hydra.lib.logic.not_(hydra.lib.equality.equal(compare_precision(hydra.reflect.integer_type_precision(source), hydra.reflect.integer_type_precision(target)), hydra.util.Comparison.LESS_THAN))
        def step[T5, T6]() -> hydra.compute.Coder[T5, T6, hydra.core.IntegerValue, hydra.core.IntegerValue]:
            return cast(hydra.compute.Coder[T5, T6, hydra.core.IntegerValue, hydra.core.IntegerValue], hydra.compute.Coder((lambda iv: hydra.lib.flows.pure(convert_integer_value(target, iv))), (lambda iv: hydra.lib.flows.pure(convert_integer_value(source, iv)))))
        def msg() -> str:
            return disclaimer(lossy(), hydra.show.core.integer_type(source), hydra.show.core.integer_type(target))
        return hydra.monads.warn(msg(), hydra.lib.flows.pure(cast(hydra.compute.Adapter[T3, T4, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue], hydra.compute.Adapter(lossy(), source, target, cast(hydra.compute.Coder[T3, T4, hydra.core.IntegerValue, hydra.core.IntegerValue], step())))))
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
    def alts[T2, T3, T4](t: hydra.core.IntegerType) -> hydra.compute.Flow[T2, frozenlist[hydra.compute.Adapter[T3, T4, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]]:
        return hydra.lib.flows.map_list((lambda v1: make_adapter(t, v1)), alt_types(t))
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (supported := (lambda v1: hydra.adapt.utils.integer_type_is_supported(cx.language.constraints, v1)), hydra.adapt.utils.choose_adapter(cast(Callable[[hydra.core.IntegerType], hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T0, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue]]]], (lambda x1: alts(x1))), supported, hydra.show.core.integer_type, hydra.show.core.integer_type, it))[1]))

def literal_adapter[T0](lt: hydra.core.LiteralType) -> hydra.compute.Flow[hydra.coders.AdapterContext, hydra.compute.Adapter[T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]:
    def for_binary[T1, T2, T3, T4](t: T1) -> hydra.compute.Flow[T2, frozenlist[hydra.compute.Adapter[T3, T4, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def match_binary[T5](lit: hydra.core.Literal) -> hydra.compute.Flow[T5, hydra.core.Literal]:
            match lit:
                case hydra.core.LiteralBinary(value=b):
                    return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralString(hydra.lib.literals.binary_to_string(b))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        def match_string[T5](lit: hydra.core.Literal) -> hydra.compute.Flow[T5, hydra.core.Literal]:
            match lit:
                case hydra.core.LiteralString(value=s):
                    return hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralBinary(hydra.lib.literals.string_to_binary(s))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        def step[T5, T6]() -> hydra.compute.Coder[T5, T6, hydra.core.Literal, hydra.core.Literal]:
            return cast(hydra.compute.Coder[T5, T6, hydra.core.Literal, hydra.core.Literal], hydra.compute.Coder(cast(Callable[[hydra.core.Literal], hydra.compute.Flow[T5, hydra.core.Literal]], (lambda x1: match_binary(x1))), cast(Callable[[hydra.core.Literal], hydra.compute.Flow[T6, hydra.core.Literal]], (lambda x1: match_string(x1)))))
        return hydra.lib.flows.pure((cast(hydra.compute.Adapter[T3, T4, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal], hydra.compute.Adapter(False, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeString()), cast(hydra.compute.Coder[T3, T4, hydra.core.Literal, hydra.core.Literal], step()))),))
    def for_boolean[T1, T2, T3](t: T1) -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def match_boolean[T4, T5](step_: hydra.compute.Coder[T4, T5, hydra.core.IntegerValue, hydra.core.IntegerValue], lit: hydra.core.Literal) -> hydra.compute.Flow[T4, hydra.core.Literal]:
            match lit:
                case hydra.core.LiteralBoolean(value=bv):
                    return hydra.lib.flows.bind(step_.encode(cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(hydra.lib.logic.if_else(bv, (lambda : 1), (lambda : 0))))), (lambda iv: hydra.lib.flows.pure(cast(hydra.core.Literal, hydra.core.LiteralInteger(iv)))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        def match_integer[T4, T5](step_: hydra.compute.Coder[T4, T5, hydra.core.IntegerValue, hydra.core.IntegerValue], lit: hydra.core.Literal) -> hydra.compute.Flow[T5, hydra.core.Literal]:
            def for_value(val: hydra.core.IntegerValue) -> hydra.core.Literal:
                match val:
                    case hydra.core.IntegerValueUint8(value=v):
                        return cast(hydra.core.Literal, hydra.core.LiteralBoolean(hydra.lib.equality.equal(v, 1)))
                    
                    case _:
                        raise TypeError("Unsupported IntegerValue")
            match lit:
                case hydra.core.LiteralInteger(value=iv):
                    return hydra.lib.flows.bind(step_.decode(iv), (lambda val: hydra.lib.flows.pure(for_value(val))))
                
                case _:
                    raise TypeError("Unsupported Literal")
        return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (constraints := cx.language.constraints, has_integers := (lambda : hydra.lib.logic.not_(hydra.lib.sets.null(constraints.integer_types))), has_strings := (lambda : hydra.lib.sets.member(hydra.variants.LiteralVariant.STRING, constraints.literal_variants)), with_integers := (lambda : "let terms are not supported here"), with_strings := (lambda : "let terms are not supported here"), hydra.lib.logic.if_else(has_integers(), (lambda : cast(hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]], with_integers())), (lambda : hydra.lib.logic.if_else(has_strings(), (lambda : hydra.lib.flows.pure(cast(frozenlist[hydra.compute.Adapter[T2, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]], with_strings()))), (lambda : hydra.lib.flows.fail("no alternatives available for boolean encoding"))))))[5]))
    def for_float[T1, T2](t: T1, ft: hydra.core.FloatType) -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T2, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def with_floats[T3]() -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T3, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
            def adapt[T4, T5, T6](adapter: hydra.compute.Adapter[T4, T4, T5, T6, hydra.core.FloatValue, hydra.core.FloatValue], dir: hydra.coders.CoderDirection, l: hydra.core.Literal) -> hydra.compute.Flow[T4, hydra.core.Literal]:
                match l:
                    case hydra.core.LiteralFloat(value=fv):
                        return hydra.lib.flows.map((lambda x: cast(hydra.core.Literal, hydra.core.LiteralFloat(x))), hydra.adapt.utils.encode_decode(dir, adapter.coder, fv))
                    
                    case _:
                        return hydra.monads.unexpected("floating-point literal", hydra.show.core.literal(l))
            return hydra.lib.flows.bind(float_adapter(ft), (lambda adapter: (step := (lambda : hydra.adapt.utils.bidirectional((lambda v1, v2: adapt(adapter, v1, v2)))), hydra.lib.flows.pure((cast(hydra.compute.Adapter[T3, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal], hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeFloat(adapter.target)), step())),)))[1]))
        return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (constraints := cx.language.constraints, has_floats := (lambda : hydra.lib.logic.not_(hydra.lib.sets.null(constraints.float_types))), hydra.lib.logic.if_else(has_floats(), (lambda : cast(hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T2, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]], with_floats())), (lambda : hydra.lib.flows.fail("no float types available"))))[2]))
    def for_integer[T1, T2](t: T1, it: hydra.core.IntegerType) -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T2, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
        def with_integers[T3]() -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T3, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
            def adapt[T4, T5, T6](adapter: hydra.compute.Adapter[T4, T4, T5, T6, hydra.core.IntegerValue, hydra.core.IntegerValue], dir: hydra.coders.CoderDirection, lit: hydra.core.Literal) -> hydra.compute.Flow[T4, hydra.core.Literal]:
                match lit:
                    case hydra.core.LiteralInteger(value=iv):
                        return hydra.lib.flows.map((lambda x: cast(hydra.core.Literal, hydra.core.LiteralInteger(x))), hydra.adapt.utils.encode_decode(dir, adapter.coder, iv))
                    
                    case _:
                        return hydra.monads.unexpected("integer literal", hydra.show.core.literal(lit))
            return hydra.lib.flows.bind(integer_adapter(it), (lambda adapter: (step := (lambda : hydra.adapt.utils.bidirectional((lambda v1, v2: adapt(adapter, v1, v2)))), hydra.lib.flows.pure((cast(hydra.compute.Adapter[T3, T3, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal], hydra.compute.Adapter(adapter.is_lossy, t, cast(hydra.core.LiteralType, hydra.core.LiteralTypeInteger(adapter.target)), step())),)))[1]))
        return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (constraints := cx.language.constraints, has_integers := (lambda : hydra.lib.logic.not_(hydra.lib.sets.null(constraints.integer_types))), hydra.lib.logic.if_else(has_integers(), (lambda : cast(hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T2, T2, T1, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]], with_integers())), (lambda : hydra.lib.flows.fail("no integer types available"))))[2]))
    def alts[T1](t: hydra.core.LiteralType) -> hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T1, T1, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]:
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
                return hydra.lib.flows.fail("no substitute for the literal string type")
            
            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.flows.bind(cast(hydra.compute.Flow[hydra.coders.AdapterContext, hydra.coders.AdapterContext], hydra.monads.get_state()), (lambda cx: (supported := (lambda v1: hydra.adapt.utils.literal_type_is_supported(cx.language.constraints, v1)), hydra.adapt.utils.choose_adapter(cast(Callable[[hydra.core.LiteralType], hydra.compute.Flow[hydra.coders.AdapterContext, frozenlist[hydra.compute.Adapter[T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal]]]], (lambda x1: alts(x1))), supported, hydra.show.core.literal_type, hydra.show.core.literal_type, lt))[1]))
