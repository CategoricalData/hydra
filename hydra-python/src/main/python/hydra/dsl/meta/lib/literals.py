"""Phantom-typed term DSL for the hydra.lib.literals library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1


def bigfloat_to_bigint(x: TTerm) -> TTerm:
    """Convert a Decimal to an int."""
    return primitive1(x)


def bigfloat_to_float32(x: TTerm) -> TTerm:
    """Convert a Decimal to a float."""
    return primitive1(x)


def bigfloat_to_float64(x: TTerm) -> TTerm:
    """Convert a Decimal to a float."""
    return primitive1(x)


def bigint_to_bigfloat(x: TTerm) -> TTerm:
    """Convert an int to a Decimal."""
    return primitive1(x)


def bigint_to_int8(x: TTerm) -> TTerm:
    """Convert an int to int8."""
    return primitive1(x)


def bigint_to_int16(x: TTerm) -> TTerm:
    """Convert an int to int16."""
    return primitive1(x)


def bigint_to_int32(x: TTerm) -> TTerm:
    """Convert an int to int32."""
    return primitive1(x)


def bigint_to_int64(x: TTerm) -> TTerm:
    """Convert an int to int64."""
    return primitive1(x)


def bigint_to_uint8(x: TTerm) -> TTerm:
    """Convert an int to uint8."""
    return primitive1(x)


def bigint_to_uint16(x: TTerm) -> TTerm:
    """Convert an int to uint16."""
    return primitive1(x)


def bigint_to_uint32(x: TTerm) -> TTerm:
    """Convert an int to uint32."""
    return primitive1(x)


def bigint_to_uint64(x: TTerm) -> TTerm:
    """Convert an int to uint64."""
    return primitive1(x)


def binary_to_string(s: TTerm) -> TTerm:
    """Convert binary to string by decoding bytes to UTF-8."""
    return primitive1(s)


def float32_to_bigfloat(x: TTerm) -> TTerm:
    """Convert a float to a Decimal."""
    return primitive1(x)


def float64_to_bigfloat(x: TTerm) -> TTerm:
    """Convert a float to a Decimal."""
    return primitive1(x)


def int8_to_bigint(x: TTerm) -> TTerm:
    """Convert int8 to int."""
    return primitive1(x)


def int16_to_bigint(x: TTerm) -> TTerm:
    """Convert int16 to int."""
    return primitive1(x)


def int32_to_bigint(x: TTerm) -> TTerm:
    """Convert int32 to int."""
    return primitive1(x)


def int64_to_bigint(x: TTerm) -> TTerm:
    """Convert int64 to int."""
    return primitive1(x)


def read_bigfloat(s: TTerm) -> TTerm:
    """Parse a string to a Decimal."""
    return primitive1(s)


def read_boolean(s: TTerm) -> TTerm:
    """Parse a string to a boolean."""
    return primitive1(s)


def read_float32(s: TTerm) -> TTerm:
    """Parse a string to a float."""
    return primitive1(s)


def read_float64(s: TTerm) -> TTerm:
    """Parse a string to a float."""
    return primitive1(s)


def read_int32(s: TTerm) -> TTerm:
    """Parse a string to an int."""
    return primitive1(s)


def read_int64(s: TTerm) -> TTerm:
    """Parse a string to an int."""
    return primitive1(s)


def read_string(s: TTerm) -> TTerm:
    """Parse a string literal."""
    return primitive1(s)


def show_bigfloat(x: TTerm) -> TTerm:
    """Convert a Decimal to string."""
    return primitive1(x)


def show_bigint(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_boolean(b: TTerm) -> TTerm:
    """Convert a boolean to string."""
    return primitive1(b)


def show_float32(x: TTerm) -> TTerm:
    """Convert a float to string."""
    return primitive1(x)


def show_float64(x: TTerm) -> TTerm:
    """Convert a float to string."""
    return primitive1(x)


def show_int8(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int16(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int32(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int64(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint8(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint16(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint32(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint64(x: TTerm) -> TTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_string(s: TTerm) -> TTerm:
    """Convert a string to a quoted string representation."""
    return primitive1(s)


def string_to_binary(s: TTerm) -> TTerm:
    """Convert string to binary by encoding string to UTF-8 bytes."""
    return primitive1(s)


def uint8_to_bigint(x: TTerm) -> TTerm:
    """Convert uint8 to int."""
    return primitive1(x)


def uint16_to_bigint(x: TTerm) -> TTerm:
    """Convert uint16 to int."""
    return primitive1(x)


def uint32_to_bigint(x: TTerm) -> TTerm:
    """Convert uint32 to int."""
    return primitive1(x)


def uint64_to_bigint(x: TTerm) -> TTerm:
    """Convert uint64 to int."""
    return primitive1(x)
