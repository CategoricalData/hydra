"""A DSL for constructing literal terms using Python's built-in datatypes."""

from decimal import Decimal

import hydra.dsl.terms as terms
from hydra.phantoms import TTerm


def string(value: str) -> TTerm[str]:
    """Construct a string term."""
    return TTerm[str](terms.string(value))


def char(value: str) -> TTerm[int]:
    """Construct a character term (represented as int32 code point)."""
    return TTerm[int](terms.char(value))


def decimal(value: Decimal) -> TTerm[Decimal]:
    """Construct a decimal (arbitrary-precision) term."""
    return TTerm[Decimal](terms.decimal(value))


def int_(value: int) -> TTerm[int]:
    """Construct a default-width integer term (alias for int32)."""
    return int32(value)


def uint8(value: int) -> TTerm[int]:
    """Construct a uint8 term."""
    return TTerm[int](terms.uint8(value))


def uint16(value: int) -> TTerm[int]:
    """Construct a uint16 term."""
    return TTerm[int](terms.uint16(value))


def uint32(value: int) -> TTerm[int]:
    """Construct a uint32 term."""
    return TTerm[int](terms.uint32(value))


def uint64(value: int) -> TTerm[int]:
    """Construct a uint64 term."""
    return TTerm[int](terms.uint64(value))


def boolean(value: bool) -> TTerm[bool]:
    """Construct a boolean term."""
    return TTerm[bool](terms.boolean(value))


def true() -> TTerm[bool]:
    """Construct a true term."""
    return boolean(True)


def false() -> TTerm[bool]:
    """Construct a false term."""
    return boolean(False)


def bigint(value: int) -> TTerm[int]:
    """Construct a bigint term."""
    return TTerm[int](terms.bigint(value))


def binary(value: bytes) -> TTerm[bytes]:
    """Construct a binary term."""
    return TTerm[bytes](terms.binary(value))


def double(value: float) -> TTerm[float]:
    """Construct a double term."""
    return TTerm[float](terms.float64(value))


def float_(value: float) -> TTerm[float]:
    """Construct a float term."""
    return float32(value)


def float32(value: float) -> TTerm[float]:
    """Construct a float32 term."""
    return TTerm[float](terms.float32(value))


def float64(value: float) -> TTerm[float]:
    """Construct a float64 term."""
    return TTerm[float](terms.float64(value))


def integer(value: int) -> TTerm[int]:
    """Construct an int term."""
    return int32(value)


def int8(value: int) -> TTerm[int]:
    """Construct an int8 term."""
    return TTerm[int](terms.int8(value))


def int16(value: int) -> TTerm[int]:
    """Construct an int16 term."""
    return TTerm[int](terms.int16(value))


def int32(value: int) -> TTerm[int]:
    """Construct an int32 term."""
    return TTerm[int](terms.int32(value))


def int64(value: int) -> TTerm[int]:
    """Construct an int64 term."""
    return TTerm[int](terms.int64(value))
