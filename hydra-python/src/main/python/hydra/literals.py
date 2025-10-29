# Note: this is an automatically generated file. Do not edit.

"""Conversion functions for literal values."""

from __future__ import annotations
from decimal import Decimal
from typing import cast
import hydra.core
import hydra.lib.literals

def bigfloat_to_float_value(ft: hydra.core.FloatType, bf: Decimal) -> hydra.core.FloatValue:
    """Convert a bigfloat to a floating-point value of a given type (note: lossy)."""
    
    match ft:
        case hydra.core.FloatType.BIGFLOAT:
            return cast(hydra.core.FloatValue, hydra.core.FloatValueBigfloat(bf))
        
        case hydra.core.FloatType.FLOAT32:
            return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat32(hydra.lib.literals.bigfloat_to_float32(bf)))
        
        case hydra.core.FloatType.FLOAT64:
            return cast(hydra.core.FloatValue, hydra.core.FloatValueFloat64(hydra.lib.literals.bigfloat_to_float64(bf)))

def bigint_to_integer_value(it: hydra.core.IntegerType, bi: int) -> hydra.core.IntegerValue:
    """Convert a bigint to an integer value of a given type (note: lossy)."""
    
    match it:
        case hydra.core.IntegerType.BIGINT:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueBigint(bi))
        
        case hydra.core.IntegerType.INT8:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt8(hydra.lib.literals.bigint_to_int8(bi)))
        
        case hydra.core.IntegerType.INT16:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt16(hydra.lib.literals.bigint_to_int16(bi)))
        
        case hydra.core.IntegerType.INT32:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(hydra.lib.literals.bigint_to_int32(bi)))
        
        case hydra.core.IntegerType.INT64:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt64(hydra.lib.literals.bigint_to_int64(bi)))
        
        case hydra.core.IntegerType.UINT8:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint8(hydra.lib.literals.bigint_to_uint8(bi)))
        
        case hydra.core.IntegerType.UINT16:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint16(hydra.lib.literals.bigint_to_uint16(bi)))
        
        case hydra.core.IntegerType.UINT32:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint32(hydra.lib.literals.bigint_to_uint32(bi)))
        
        case hydra.core.IntegerType.UINT64:
            return cast(hydra.core.IntegerValue, hydra.core.IntegerValueUint64(hydra.lib.literals.bigint_to_uint64(bi)))

def float_value_to_bigfloat(v1: hydra.core.FloatValue) -> Decimal:
    """Convert a floating-point value of any precision to a bigfloat."""
    
    match v1:
        case hydra.core.FloatValueBigfloat(value=bf):
            return bf
        
        case hydra.core.FloatValueFloat32(value=f32):
            return hydra.lib.literals.float32_to_bigfloat(f32)
        
        case hydra.core.FloatValueFloat64(value=f64):
            return hydra.lib.literals.float64_to_bigfloat(f64)

def integer_value_to_bigint(v1: hydra.core.IntegerValue) -> int:
    """Convert an integer value of any precision to a bigint."""
    
    match v1:
        case hydra.core.IntegerValueBigint(value=bi):
            return bi
        
        case hydra.core.IntegerValueInt8(value=i8):
            return hydra.lib.literals.int8_to_bigint(i8)
        
        case hydra.core.IntegerValueInt16(value=i16):
            return hydra.lib.literals.int16_to_bigint(i16)
        
        case hydra.core.IntegerValueInt32(value=i32):
            return hydra.lib.literals.int32_to_bigint(i32)
        
        case hydra.core.IntegerValueInt64(value=i64):
            return hydra.lib.literals.int64_to_bigint(i64)
        
        case hydra.core.IntegerValueUint8(value=ui8):
            return hydra.lib.literals.uint8_to_bigint(ui8)
        
        case hydra.core.IntegerValueUint16(value=ui16):
            return hydra.lib.literals.uint16_to_bigint(ui16)
        
        case hydra.core.IntegerValueUint32(value=ui32):
            return hydra.lib.literals.uint32_to_bigint(ui32)
        
        case hydra.core.IntegerValueUint64(value=ui64):
            return hydra.lib.literals.uint64_to_bigint(ui64)
