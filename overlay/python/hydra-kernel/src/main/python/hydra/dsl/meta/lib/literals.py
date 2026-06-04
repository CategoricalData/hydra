"""Phantom-typed term DSL for the hydra.lib.literals library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1


def bigint_to_decimal(x: TypedTerm) -> TypedTerm:
    """Convert an int to a decimal (arbitrary-precision exact decimal)."""
    return primitive1(x)


def bigint_to_int8(x: TypedTerm) -> TypedTerm:
    """Convert an int to int8."""
    return primitive1(x)


def bigint_to_int16(x: TypedTerm) -> TypedTerm:
    """Convert an int to int16."""
    return primitive1(x)


def bigint_to_int32(x: TypedTerm) -> TypedTerm:
    """Convert an int to int32."""
    return primitive1(x)


def bigint_to_int64(x: TypedTerm) -> TypedTerm:
    """Convert an int to int64."""
    return primitive1(x)


def bigint_to_uint8(x: TypedTerm) -> TypedTerm:
    """Convert an int to uint8."""
    return primitive1(x)


def bigint_to_uint16(x: TypedTerm) -> TypedTerm:
    """Convert an int to uint16."""
    return primitive1(x)


def bigint_to_uint32(x: TypedTerm) -> TypedTerm:
    """Convert an int to uint32."""
    return primitive1(x)


def bigint_to_uint64(x: TypedTerm) -> TypedTerm:
    """Convert an int to uint64."""
    return primitive1(x)


def binary_to_bytes(x: TypedTerm) -> TypedTerm:
    """Convert binary data to a list of byte values (0-255)."""
    return primitive1(x)


def binary_to_string(s: TypedTerm) -> TypedTerm:
    """Convert binary to string by decoding bytes to UTF-8."""
    return primitive1(s)


def decimal_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert a decimal to an int by truncating the fractional part."""
    return primitive1(x)


def decimal_to_float32(x: TypedTerm) -> TypedTerm:
    """Convert a decimal to a float32."""
    return primitive1(x)


def decimal_to_float64(x: TypedTerm) -> TypedTerm:
    """Convert a decimal to a float64."""
    return primitive1(x)


def float32_to_decimal(x: TypedTerm) -> TypedTerm:
    """Convert a float32 to a decimal."""
    return primitive1(x)


def float32_to_float64(x: TypedTerm) -> TypedTerm:
    """Convert a float32 to a float64 (lossless widening)."""
    return primitive1(x)


def float64_to_decimal(x: TypedTerm) -> TypedTerm:
    """Convert a float64 to a decimal."""
    return primitive1(x)


def float64_to_float32(x: TypedTerm) -> TypedTerm:
    """Convert a float64 to a float32 (lossy narrowing)."""
    return primitive1(x)


def int8_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert int8 to int."""
    return primitive1(x)


def int16_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert int16 to int."""
    return primitive1(x)


def int32_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert int32 to int."""
    return primitive1(x)


def int64_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert int64 to int."""
    return primitive1(x)


def read_boolean(s: TypedTerm) -> TypedTerm:
    """Parse a string to a boolean."""
    return primitive1(s)


def read_decimal(s: TypedTerm) -> TypedTerm:
    """Parse a string to a decimal (arbitrary-precision exact decimal)."""
    return primitive1(s)


def read_float32(s: TypedTerm) -> TypedTerm:
    """Parse a string to a float."""
    return primitive1(s)


def read_float64(s: TypedTerm) -> TypedTerm:
    """Parse a string to a float."""
    return primitive1(s)


def read_int32(s: TypedTerm) -> TypedTerm:
    """Parse a string to an int."""
    return primitive1(s)


def read_int64(s: TypedTerm) -> TypedTerm:
    """Parse a string to an int."""
    return primitive1(s)


def read_string(s: TypedTerm) -> TypedTerm:
    """Parse a string literal."""
    return primitive1(s)


def show_bigint(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_boolean(b: TypedTerm) -> TypedTerm:
    """Convert a boolean to string."""
    return primitive1(b)


def show_decimal(x: TypedTerm) -> TypedTerm:
    """Convert a decimal (arbitrary-precision exact decimal) to its string representation."""
    return primitive1(x)


def show_float32(x: TypedTerm) -> TypedTerm:
    """Convert a float to string."""
    return primitive1(x)


def show_float64(x: TypedTerm) -> TypedTerm:
    """Convert a float to string."""
    return primitive1(x)


def show_int8(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int16(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int32(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_int64(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint8(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint16(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint32(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_uint64(x: TypedTerm) -> TypedTerm:
    """Convert an int to string."""
    return primitive1(x)


def show_string(s: TypedTerm) -> TypedTerm:
    """Convert a string to a quoted string representation."""
    return primitive1(s)


def string_to_binary(s: TypedTerm) -> TypedTerm:
    """Convert string to binary by encoding string to UTF-8 bytes."""
    return primitive1(s)


def uint8_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert uint8 to int."""
    return primitive1(x)


def uint16_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert uint16 to int."""
    return primitive1(x)


def uint32_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert uint32 to int."""
    return primitive1(x)


def uint64_to_bigint(x: TypedTerm) -> TypedTerm:
    """Convert uint64 to int."""
    return primitive1(x)
