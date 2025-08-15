"""Conversion functions for literal values."""

from __future__ import annotations
import hydra.core
import hydra.lib.literals

def float_value_to_bigfloat(v1: hydra.core.FloatValue) -> float:
    match v1:
        case hydra.core.FloatValueBigfloat(f):
            return f
        
        case hydra.core.FloatValueFloat32(v1):
            return hydra.lib.literals.float32_to_bigfloat(v1)
        
        case hydra.core.FloatValueFloat64(v1):
            return hydra.lib.literals.float64_to_bigfloat(v1)

def integer_value_to_bigint(v1: hydra.core.IntegerValue) -> int:
    """Convert an integer value of any precision to a bigint."""
    
    match v1:
        case hydra.core.IntegerValueBigint(i):
            return i
        
        case hydra.core.IntegerValueInt8(v1):
            return hydra.lib.literals.int8_to_bigint(v1)
        
        case hydra.core.IntegerValueInt16(v1):
            return hydra.lib.literals.int16_to_bigint(v1)
        
        case hydra.core.IntegerValueInt32(v1):
            return hydra.lib.literals.int32_to_bigint(v1)
        
        case hydra.core.IntegerValueInt64(v1):
            return hydra.lib.literals.int64_to_bigint(v1)
        
        case hydra.core.IntegerValueUint8(v1):
            return hydra.lib.literals.uint8_to_bigint(v1)
        
        case hydra.core.IntegerValueUint16(v1):
            return hydra.lib.literals.uint16_to_bigint(v1)
        
        case hydra.core.IntegerValueUint32(v1):
            return hydra.lib.literals.uint32_to_bigint(v1)
        
        case hydra.core.IntegerValueUint64(v1):
            return hydra.lib.literals.uint64_to_bigint(v1)
