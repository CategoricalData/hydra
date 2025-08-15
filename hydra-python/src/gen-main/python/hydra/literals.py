"""Conversion functions for literal values."""

from __future__ import annotations
import hydra.core
import hydra.lib.literals

def float_value_to_bigfloat(v1: hydra.core.FloatValue) -> float:
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
