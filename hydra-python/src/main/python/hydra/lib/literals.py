"""Python implementations of hydra.lib.literals primitives."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Maybe, Just, NOTHING


def bigfloat_to_bigint(x: Decimal) -> int:
    """Convert a Decimal to an int."""
    return round(x)


def bigfloat_to_float32(x: Decimal) -> float:
    """Convert a Decimal to a float32 (single precision).

    Python's float is float64, so we need to explicitly convert through
    float32 to get the correct precision.
    """
    import struct
    # Pack as float32, unpack as float32 to get proper rounding
    f64 = float(x)
    f32_bytes = struct.pack('f', f64)
    f32 = struct.unpack('f', f32_bytes)[0]
    return f32


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
    """Convert binary to string by base64 encoding."""
    import base64
    return base64.b64encode(s).decode('ascii')


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


def read_bigint(s: str) -> Maybe[int]:
    """Parse a string to an integer."""
    try:
        return Just(int(s))
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
    """Parse a string to a float32 (single precision)."""
    import struct
    try:
        f64 = float(s)
        # Convert to float32 precision
        f32_bytes = struct.pack('f', f64)
        f32 = struct.unpack('f', f32_bytes)[0]
        return Just(f32)
    except:
        return NOTHING


def read_float64(s: str) -> Maybe[float]:
    """Parse a string to a float."""
    try:
        return Just(float(s))
    except:
        return NOTHING


def read_int8(s: str) -> Maybe[int]:
    """Parse a string to an int8 (-128 to 127)."""
    try:
        n = int(s)
        if n >= -128 and n <= 127:
            return Just(n)
        else:
            return NOTHING
    except:
        return NOTHING


def read_int16(s: str) -> Maybe[int]:
    """Parse a string to an int16 (-32768 to 32767)."""
    try:
        n = int(s)
        if n >= -32768 and n <= 32767:
            return Just(n)
        else:
            return NOTHING
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


def read_uint8(s: str) -> Maybe[int]:
    """Parse a string to a uint8 (0 to 255)."""
    try:
        n = int(s)
        if n >= 0 and n <= 255:
            return Just(n)
        else:
            return NOTHING
    except:
        return NOTHING


def read_uint16(s: str) -> Maybe[int]:
    """Parse a string to a uint16 (0 to 65535)."""
    try:
        n = int(s)
        if n >= 0 and n <= 65535:
            return Just(n)
        else:
            return NOTHING
    except:
        return NOTHING


def read_uint32(s: str) -> Maybe[int]:
    """Parse a string to a uint32 (0 to 4294967295)."""
    try:
        n = int(s)
        if n >= 0 and n <= 4294967295:
            return Just(n)
        else:
            return NOTHING
    except:
        return NOTHING


def read_uint64(s: str) -> Maybe[int]:
    """Parse a string to a uint64 (0 to 18446744073709551615)."""
    try:
        n = int(s)
        if n >= 0 and n <= 18446744073709551615:
            return Just(n)
        else:
            return NOTHING
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
    """Convert a float32 to string with appropriate precision.

    Float32 has about 6-7 significant decimal digits. We format with
    enough precision to distinguish float32 values while avoiding
    spurious digits from float64 representation.
    """
    # Format with 7 significant figures
    formatted = f"{x:.7g}"
    # Ensure we have a decimal point for whole numbers (like Haskell)
    if '.' not in formatted and 'e' not in formatted and 'E' not in formatted:
        formatted += ".0"
    return formatted


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
    """Convert a string to a quoted string representation (with double quotes)."""
    # Use Haskell-style double quotes instead of Python's single quotes
    escaped = s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t')
    return f'"{escaped}"'


def string_to_binary(s: str) -> bytes:
    """Convert string to binary by base64 decoding."""
    import base64
    return base64.b64decode(s)


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
