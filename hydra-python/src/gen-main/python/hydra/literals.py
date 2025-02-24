"""Conversion functions for literal values."""

from __future__ import annotations
import hydra.core
import hydra.lib.equality
import hydra.lib.literals

def floatValueToBigfloat(v1) :
    match v1:
        case hydra.core.FloatValueBigfloat(f):
            return hydra.lib.equality.identity(f)
        
        case hydra.core.FloatValueFloat32(v1):
            return hydra.lib.literals.float32ToBigfloat(v1)
        
        case hydra.core.FloatValueFloat64(v1):
            return hydra.lib.literals.float64ToBigfloat(v1)
        
        case _:
            raise TypeError("Unsupported FloatValue")

def integerValueToBigint(v1) :
    """Convert an integer value of any precision to a bigint."""
    
    match v1:
        case hydra.core.IntegerValueBigint(i):
            return hydra.lib.equality.identity(i)
        
        case hydra.core.IntegerValueInt8(v1):
            return hydra.lib.literals.int8ToBigint(v1)
        
        case hydra.core.IntegerValueInt16(v1):
            return hydra.lib.literals.int16ToBigint(v1)
        
        case hydra.core.IntegerValueInt32(v1):
            return hydra.lib.literals.int32ToBigint(v1)
        
        case hydra.core.IntegerValueInt64(v1):
            return hydra.lib.literals.int64ToBigint(v1)
        
        case hydra.core.IntegerValueUint8(v1):
            return hydra.lib.literals.uint8ToBigint(v1)
        
        case hydra.core.IntegerValueUint16(v1):
            return hydra.lib.literals.uint16ToBigint(v1)
        
        case hydra.core.IntegerValueUint32(v1):
            return hydra.lib.literals.uint32ToBigint(v1)
        
        case hydra.core.IntegerValueUint64(v1):
            return hydra.lib.literals.uint64ToBigint(v1)
        
        case _:
            raise TypeError("Unsupported IntegerValue")