"""A DSL for constructing Hydra literal values in Python."""

from hydra.core import (
    Literal,
    FloatValue,
    IntegerValue,
    LiteralBinary,
    LiteralBoolean,
    LiteralFloat,
    LiteralInteger,
    LiteralString,
    FloatValueBigfloat,
    IntegerValueBigint,
    FloatValueFloat32,
    FloatValueFloat64,
    IntegerValueInt16,
    IntegerValueInt32,
    IntegerValueInt64,
    IntegerValueInt8,
    IntegerValueUint16,
    IntegerValueUint32,
    IntegerValueUint64,
    IntegerValueUint8,
)

from decimal import Decimal


def binary(value: bytes) -> Literal:
    """Create a binary data literal.

    Example: binary(b'\\x48\\x65\\x6C\\x6C\\x6F')
    """
    return LiteralBinary(value)


def boolean(value: bool) -> Literal:
    """Create a boolean literal.

    Example: boolean(True)
    """
    return LiteralBoolean(value)


def string(value: str) -> Literal:
    """Create a string literal.

    Example: string("hello world")
    """
    return LiteralString(value)


def float_(value: FloatValue) -> Literal:
    """Create a floating-point literal with specified precision.

    Example: float_(FloatValueFloat32(3.14))
    """
    return LiteralFloat(value)


def float32(value: float) -> Literal:
    """Create a 32-bit floating point literal.

    Example: float32(3.14)
    """
    return float_(FloatValueFloat32(value))


def float64(value: float) -> Literal:
    """Create a 64-bit floating point literal.

    Example: float64(3.14159265359)
    """
    return float_(FloatValueFloat64(value))


def bigfloat(value: float) -> Literal:
    """Create an arbitrary-precision floating point literal.

    Example: bigfloat(3.14159265359)
    """
    # Use str(value) to avoid full binary precision conversion
    return float_(FloatValueBigfloat(Decimal(str(value))))


def integer(value: IntegerValue) -> Literal:
    """Create an integer literal with specified bit width.

    Example: integer(IntegerValueInt32(42))
    """
    return LiteralInteger(value)


def int8(value: int) -> Literal:
    """Create an 8-bit signed integer literal.

    Example: int8(127)
    """
    return integer(IntegerValueInt8(value))


def int16(value: int) -> Literal:
    """Create a 16-bit signed integer literal.

    Example: int16(32767)
    """
    return integer(IntegerValueInt16(value))


def int32(value: int) -> Literal:
    """Create a 32-bit signed integer literal.

    Example: int32(42)
    """
    return integer(IntegerValueInt32(value))


def int64(value: int) -> Literal:
    """Create a 64-bit signed integer literal.

    Example: int64(9223372036854775807)
    """
    return integer(IntegerValueInt64(value))


def bigint(value: int) -> Literal:
    """Create an arbitrary-precision integer literal.

    Example: bigint(9223372036854775808)
    """
    return integer(IntegerValueBigint(value))


def uint8(value: int) -> Literal:
    """Create an 8-bit unsigned integer literal.

    Example: uint8(255)
    """
    return integer(IntegerValueUint8(value))


def uint16(value: int) -> Literal:
    """Create a 16-bit unsigned integer literal.

    Example: uint16(65535)
    """
    return integer(IntegerValueUint16(value))


def uint32(value: int) -> Literal:
    """Create a 32-bit unsigned integer literal.

    Example: uint32(4294967295)
    """
    return integer(IntegerValueUint32(value))


def uint64(value: int) -> Literal:
    """Create a 64-bit unsigned integer literal.

    Example: uint64(18446744073709551615)
    """
    return integer(IntegerValueUint64(value))
