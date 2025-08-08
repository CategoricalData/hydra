"""Conversion functions for literal values."""

from __future__ import annotations

import hydra.gen.core
import hydra.lib.equality
import hydra.lib.literals


def float_value_to_bigfloat(v0: hydra.gen.core.FloatValue) -> float:
    match v0:
        case hydra.gen.core.FloatValueBigfloat(f):
            return hydra.lib.equality.identity(f)
        
        case hydra.gen.core.FloatValueFloat32(v1):
            return hydra.lib.literals.float32_to_bigfloat(v1)
        
        case hydra.gen.core.FloatValueFloat64(v1):
            return hydra.lib.literals.float64_to_bigfloat(v1)

def integer_value_to_bigint(v0: hydra.gen.core.IntegerValue) -> int:
    """Convert an integer value of any precision to a bigint."""
    match v0:
        case hydra.gen.core.IntegerValueBigint(i):
            return hydra.lib.equality.identity(i)
        
        case hydra.gen.core.IntegerValueInt8(v1):
            return hydra.lib.literals.int8_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueInt16(v1):
            return hydra.lib.literals.int16_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueInt32(v1):
            return hydra.lib.literals.int32_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueInt64(v1):
            return hydra.lib.literals.int64_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueUint8(v1):
            return hydra.lib.literals.uint8_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueUint16(v1):
            return hydra.lib.literals.uint16_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueUint32(v1):
            return hydra.lib.literals.uint32_to_bigint(v1)
        
        case hydra.gen.core.IntegerValueUint64(v1):
            return hydra.lib.literals.uint64_to_bigint(v1)
