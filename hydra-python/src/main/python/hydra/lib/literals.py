"""Python implementations of hydra.lib.literals primitives."""

import struct


def bigfloat_to_bigint(x: float) -> int:
    """Convert a logical bigfloat to a logical bigint."""
    return round(x)


def bigfloat_to_float32(x: float) -> float:
    """Convert a logical bigfloat to a logical float32."""
    return struct.unpack("f", struct.pack("f", x))[0]


def bigfloat_to_float64(x: float) -> float:
    """Convert a logical bigfloat to a logical float64."""
    return x


def bigint_to_bigfloat(x: int) -> float:
    """Convert a logical bigint to a logical bigfloat."""
    return float(x)


def bigint_to_int8(x: int) -> int:
    """Convert a logical bigint to a logical int8."""
    return struct.unpack("b", struct.pack("b", x))[0]


def bigint_to_int16(x: int) -> int:
    """Convert a logical bigint to a logical int16."""
    return struct.unpack("h", struct.pack("h", x))[0]


def bigint_to_int32(x: int) -> int:
    """Convert a logical bigint to a logical int32."""
    return struct.unpack("i", struct.pack("i", x))[0]


def bigint_to_int64(x: int) -> int:
    """Convert a logical bigint to a logical int64."""
    return struct.unpack("q", struct.pack("q", x))[0]


def bigint_to_uint8(x: int) -> int:
    """Convert a logical bigint to a logical uint8."""
    return struct.unpack("B", struct.pack("B", x))[0]


def bigint_to_uint16(x: int) -> int:
    """Convert a logical bigint to a logical uint16."""
    return struct.unpack("H", struct.pack("H", x))[0]


def bigint_to_uint32(x: int) -> int:
    """Convert a logical bigint to a logical uint32."""
    return struct.unpack("I", struct.pack("I", x))[0]


def bigint_to_uint64(x: int) -> int:
    """Convert a logical bigint to a logical uint64."""
    return struct.unpack("Q", struct.pack("Q", x))[0]


def float32_to_bigfloat(x: float) -> float:
    """Convert a logical float32 to a logical bigfloat."""
    return x


def float64_to_bigfloat(x: float) -> float:
    """Convert a logical float64 to a logical bigfloat."""
    return x


def int8_to_bigint(x: int) -> int:
    """Convert a logical int8 to a logical bigint."""
    return x


def int16_to_bigint(x: int) -> int:
    """Convert a logical int16 to a logical bigint."""
    return x


def int32_to_bigint(x: int) -> int:
    """Convert a logical int32 to a logical bigint."""
    return x


def int64_to_bigint(x: int) -> int:
    """Convert a logical int64 to a logical bigint."""
    return x


def show_int32(x: int) -> str:
    """Convert a logical int32 to a printable string."""
    return str(x)


def show_string(x: str) -> str:
    """Convert a string to a quoted string."""
    return x


def uint8_to_bigint(x: int) -> int:
    """Convert a logical uint8 to a logical bigint."""
    return x


def uint16_to_bigint(x: int) -> int:
    """Convert a logical uint16 to a logical bigint."""
    return x


def uint32_to_bigint(x: int) -> int:
    """Convert a logical uint32 to a logical bigint."""
    return x


def uint64_to_bigint(x: int) -> int:
    """Convert a logical uint64 to a logical bigint."""
    return x
