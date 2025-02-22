"""Python implementations of hydra.lib.literals primitives."""

import struct


def bigfloat_to_bigint(x: float) -> int:
    """Convert a double to an integer."""
    return round(x)


def bigfloat_to_float32(x: float) -> float:
    """Convert a double to a float."""
    return struct.unpack("f", struct.pack("f", x))[0]


def bigfloat_to_float64(x: float) -> float:
    """Convert a double to a double."""
    return x


def bigint_to_bigfloat(x: int) -> float:
    """Convert an integer to a double."""
    return float(x)


def bigint_to_int8(x: int) -> int:
    """Convert an integer to an int8."""
    return struct.unpack("b", struct.pack("b", x))[0]


def bigint_to_int16(x: int) -> int:
    """Convert an integer to an int16."""
    return struct.unpack("h", struct.pack("h", x))[0]


def bigint_to_int32(x: int) -> int:
    """Convert an integer to an int."""
    return struct.unpack("i", struct.pack("i", x))[0]


def bigint_to_int64(x: int) -> int:
    """Convert an integer to an int64."""
    return struct.unpack("q", struct.pack("q", x))[0]


def bigint_to_uint8(x: int) -> int:
    """Convert an integer to an int16."""
    return struct.unpack("B", struct.pack("B", x))[0]


def bigint_to_uint16(x: int) -> int:
    """Convert an integer to an uint16."""
    return struct.unpack("H", struct.pack("H", x))[0]


def bigint_to_uint32(x: int) -> int:
    """Convert an integer to an uint64."""
    return struct.unpack("I", struct.pack("I", x))[0]


def bigint_to_uint64(x: int) -> int:
    """Convert an integer to an uint64."""
    return struct.unpack("Q", struct.pack("Q", x))[0]


def float32_to_bigfloat(x: float) -> float:
    """Convert a float to a double."""
    return x


def float64_to_bigfloat(x: float) -> float:
    """Convert a double to a double."""
    return x


def int8_to_bigint(x: int) -> int:
    """Convert an int8 to an integer."""
    return x


def int16_to_bigint(x: int) -> int:
    """Convert an int16 to an integer."""
    return x


def int32_to_bigint(x: int) -> int:
    """Convert an int to an integer."""
    return x


def int64_to_bigint(x: int) -> int:
    """Convert an int64 to an integer."""
    return x


def show_int32(x: int) -> str:
    """Convert an int to a string."""
    return str(x)


def show_string(x: str) -> str:
    """Convert a string to a string."""
    return x


def uint16_to_bigint(x: int) -> int:
    """Convert an int to an integer."""
    return x


def uint32_to_bigint(x: int) -> int:
    """Convert an int64 to an integer."""
    return x


def uint64_to_bigint(x: int) -> int:
    """Convert an integer to an integer."""
    return x
