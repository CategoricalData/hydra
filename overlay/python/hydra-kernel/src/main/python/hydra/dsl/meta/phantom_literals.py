"""A DSL for constructing literal terms using Python's built-in datatypes."""

from decimal import Decimal

import hydra.dsl.terms as terms
from hydra.typed import TypedTerm


def string(value: str) -> TypedTerm[str]:
    """Construct a string term."""
    return TypedTerm[str](terms.string(value))


def char(value: str) -> TypedTerm[int]:
    """Construct a character term (represented as int32 code point)."""
    return TypedTerm[int](terms.char(value))


def decimal(value: Decimal) -> TypedTerm[Decimal]:
    """Construct a decimal (arbitrary-precision) term."""
    return TypedTerm[Decimal](terms.decimal(value))


def int_(value: int) -> TypedTerm[int]:
    """Construct a default-width integer term (alias for int32)."""
    return int32(value)


def uint8(value: int) -> TypedTerm[int]:
    """Construct a uint8 term."""
    return TypedTerm[int](terms.uint8(value))


def uint16(value: int) -> TypedTerm[int]:
    """Construct a uint16 term."""
    return TypedTerm[int](terms.uint16(value))


def uint32(value: int) -> TypedTerm[int]:
    """Construct a uint32 term."""
    return TypedTerm[int](terms.uint32(value))


def uint64(value: int) -> TypedTerm[int]:
    """Construct a uint64 term."""
    return TypedTerm[int](terms.uint64(value))


def boolean(value: bool) -> TypedTerm[bool]:
    """Construct a boolean term."""
    return TypedTerm[bool](terms.boolean(value))


def true() -> TypedTerm[bool]:
    """Construct a true term."""
    return boolean(True)


def false() -> TypedTerm[bool]:
    """Construct a false term."""
    return boolean(False)


def bigint(value: int) -> TypedTerm[int]:
    """Construct a bigint term."""
    return TypedTerm[int](terms.bigint(value))


def binary(value: bytes) -> TypedTerm[bytes]:
    """Construct a binary term."""
    return TypedTerm[bytes](terms.binary(value))


def double(value: float) -> TypedTerm[float]:
    """Construct a double term."""
    return TypedTerm[float](terms.float64(value))


def float_(value: float) -> TypedTerm[float]:
    """Construct a float term."""
    return float32(value)


def float32(value: float) -> TypedTerm[float]:
    """Construct a float32 term."""
    return TypedTerm[float](terms.float32(value))


def float64(value: float) -> TypedTerm[float]:
    """Construct a float64 term."""
    return TypedTerm[float](terms.float64(value))


def integer(value: int) -> TypedTerm[int]:
    """Construct an int term."""
    return int32(value)


def int8(value: int) -> TypedTerm[int]:
    """Construct an int8 term."""
    return TypedTerm[int](terms.int8(value))


def int16(value: int) -> TypedTerm[int]:
    """Construct an int16 term."""
    return TypedTerm[int](terms.int16(value))


def int32(value: int) -> TypedTerm[int]:
    """Construct an int32 term."""
    return TypedTerm[int](terms.int32(value))


def int64(value: int) -> TypedTerm[int]:
    """Construct an int64 term."""
    return TypedTerm[int](terms.int64(value))
