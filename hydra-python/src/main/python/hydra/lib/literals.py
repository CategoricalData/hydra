"""Python implementations of hydra.lib.literals primitives."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Maybe, Just, NOTHING


def bigfloat_to_bigint(x: Decimal) -> int:
    """Convert a Decimal to an int."""
    return round(x)


def bigfloat_to_float32(x: Decimal) -> float:
    """Convert a Decimal to a float."""
    return float(x)


def bigfloat_to_float64(x: Decimal) -> float:
    """Convert a Decimal to a float."""
    return float(x)


def bigint_to_bigfloat(x: int) -> Decimal:
    """Convert an int to a Decimal."""
    return Decimal(x)


def bigint_to_int8(x: int) -> int:
    """Convert an int to int8 (identity in Python)."""
    return x


def bigint_to_int16(x: int) -> int:
    """Convert an int to int16 (identity in Python)."""
    return x


def bigint_to_int32(x: int) -> int:
    """Convert an int to int32 (identity in Python)."""
    return x


def bigint_to_int64(x: int) -> int:
    """Convert an int to int64 (identity in Python)."""
    return x


def bigint_to_uint8(x: int) -> int:
    """Convert an int to uint8 (identity in Python)."""
    return x


def bigint_to_uint16(x: int) -> int:
    """Convert an int to uint16 (identity in Python)."""
    return x


def bigint_to_uint32(x: int) -> int:
    """Convert an int to uint32 (identity in Python)."""
    return x


def bigint_to_uint64(x: int) -> int:
    """Convert an int to uint64 (identity in Python)."""
    return x


def binary_to_string(s: bytes) -> str:
    """Convert binary to string by decoding bytes to UTF-8."""
    return s.decode('utf-8', errors='replace')


def float32_to_bigfloat(x: float) -> Decimal:
    """Convert a float to a Decimal."""
    return Decimal(str(x))


def float64_to_bigfloat(x: float) -> Decimal:
    """Convert a float to a Decimal."""
    return Decimal(str(x))


def int8_to_bigint(x: int) -> int:
    """Convert int8 to int (identity in Python)."""
    return x


def int16_to_bigint(x: int) -> int:
    """Convert int16 to int (identity in Python)."""
    return x


def int32_to_bigint(x: int) -> int:
    """Convert int32 to int (identity in Python)."""
    return x


def int64_to_bigint(x: int) -> int:
    """Convert int64 to int (identity in Python)."""
    return x


def read_bigfloat(s: str) -> Maybe[Decimal]:
    """Parse a string to a Decimal."""
    try:
        return Just(Decimal(s))
    except:
        return NOTHING


def read_boolean(s: str) -> Maybe[bool]:
    """Parse a string to a boolean."""
    if s == "true":
        return Just(True)
    elif s == "false":
        return Just(False)
    else:
        return NOTHING


def read_float32(s: str) -> Maybe[float]:
    """Parse a string to a float."""
    try:
        return Just(float(s))
    except:
        return NOTHING


def read_float64(s: str) -> Maybe[float]:
    """Parse a string to a float."""
    try:
        return Just(float(s))
    except:
        return NOTHING


def read_int32(s: str) -> Maybe[int]:
    """Parse a string to an int."""
    try:
        return Just(int(s))
    except:
        return NOTHING


def read_int64(s: str) -> Maybe[int]:
    """Parse a string to an int."""
    try:
        return Just(int(s))
    except:
        return NOTHING


def read_string(s: str) -> Maybe[str]:
    """Parse a string literal."""
    try:
        import ast
        result = ast.literal_eval(s)
        return Just(result) if isinstance(result, str) else NOTHING
    except:
        return NOTHING


def show_bigfloat(x: Decimal) -> str:
    """Convert a Decimal to string."""
    return str(x)


def show_bigint(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_boolean(b: bool) -> str:
    """Convert a boolean to string."""
    return "true" if b else "false"


def show_float32(x: float) -> str:
    """Convert a float to string."""
    return str(x)


def show_float64(x: float) -> str:
    """Convert a float to string."""
    return str(x)


def show_int8(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_int16(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_int32(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_int64(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_uint8(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_uint16(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_uint32(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_uint64(x: int) -> str:
    """Convert an int to string."""
    return str(x)


def show_string(s: str) -> str:
    """Convert a string to a quoted string representation."""
    return repr(s)


def string_to_binary(s: str) -> bytes:
    """Convert string to binary by encoding string to UTF-8 bytes."""
    return s.encode('utf-8', errors='replace')


def uint8_to_bigint(x: int) -> int:
    """Convert uint8 to int (identity in Python)."""
    return x


def uint16_to_bigint(x: int) -> int:
    """Convert uint16 to int (identity in Python)."""
    return x


def uint32_to_bigint(x: int) -> int:
    """Convert uint32 to int (identity in Python)."""
    return x


def uint64_to_bigint(x: int) -> int:
    """Convert uint64 to int (identity in Python)."""
    return x
