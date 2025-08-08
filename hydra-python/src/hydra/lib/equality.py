"""Python implementations of hydra.lib.equality primitives."""

from typing import Any

from hydra.core import Term, Type


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal."""
    return a == b


def equal_binary(a: str, b: str) -> bool:
    """Check if two strings are equal."""
    return a == b


def equal_boolean(a: bool, b: bool) -> bool:
    """Check if two booleans are equal."""
    return a == b


def equal_bigfloat(a: float, b: float) -> bool:
    """Check if two floats are equal."""
    return a == b


def equal_float32(a: float, b: float) -> bool:
    """Check if two floats are equal."""
    return a == b


def equal_float64(a: float, b: float) -> bool:
    """Check if two floats are equal."""
    return a == b


def equal_bigint(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_int8(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_int16(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_int32(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_int64(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_uint8(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_uint32(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_uint64(a: int, b: int) -> bool:
    """Check if two integers are equal."""
    return a == b


def equal_string(a: str, b: str) -> bool:
    """Check if two strings are equal."""
    return a == b


def equal_term(a: Term, b: Term) -> bool:
    """Check if two terms are equal."""
    return a == b


def equal_type(a: Type, b: Type) -> bool:
    """Check if two types are equal."""
    return a == b


def gt_int32(a: int, b: int) -> bool:
    """Check if one integer is greater than another."""
    return a > b


def gte_int32(a: int, b: int) -> bool:
    """Check if one integer is greater than or equal to another."""
    return a >= b


def identity(a: Any) -> Any:
    """Return the identity of a value."""
    return a


def lt_int32(a: int, b: int) -> bool:
    """Check if one integer is less than another."""
    return a < b


def lte_int32(a: int, b: int) -> bool:
    """Check if one integer is less than or equal to another."""
    return a <= b
