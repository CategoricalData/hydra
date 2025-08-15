"""Conversion functions for literal values."""

from __future__ import annotations
import hydra.core
import hydra.lib.literals

def float_value_to_bigfloat(v1: hydra.core.FloatValue) -> float:
    match v1:
        case hydra.core.FloatValueBigfloat(value=vbigfloat):
            return vbigfloat
        
        case hydra.core.FloatValueFloat32(value=vfloat32):
            return hydra.lib.literals.float32_to_bigfloat(vfloat32)
        
        case hydra.core.FloatValueFloat64(value=vfloat64):
            return hydra.lib.literals.float64_to_bigfloat(vfloat64)

def integer_value_to_bigint(v1: hydra.core.IntegerValue) -> int:
    """Convert an integer value of any precision to a bigint."""

    match v1:
        case hydra.core.IntegerValueBigint(value=vbigint):
            return vbigint

        case hydra.core.IntegerValueInt8(value=vint8):
            return hydra.lib.literals.int8_to_bigint(vint8)

        case hydra.core.IntegerValueInt16(value=vint16):
            return hydra.lib.literals.int16_to_bigint(vint16)

        case hydra.core.IntegerValueInt32(value=vint32):
            return hydra.lib.literals.int32_to_bigint(vint32)

        case hydra.core.IntegerValueInt64(value=vint64):
            return hydra.lib.literals.int64_to_bigint(vint64)

        case hydra.core.IntegerValueUint8(value=vuint8):
            return hydra.lib.literals.uint8_to_bigint(vuint8)

        case hydra.core.IntegerValueUint16(value=vuint16):
            return hydra.lib.literals.uint16_to_bigint(vuint16)

        case hydra.core.IntegerValueUint32(value=vuint32):
            return hydra.lib.literals.uint32_to_bigint(vuint32)

        case hydra.core.IntegerValueUint64(value=vuint64):
            return hydra.lib.literals.uint64_to_bigint(vuint64)
