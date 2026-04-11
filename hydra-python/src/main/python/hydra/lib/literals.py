"""Python implementations of hydra.lib.literals primitives."""

from __future__ import annotations
from decimal import Decimal
from hydra.dsl.python import Maybe, Just, NOTHING


def _format_float_like_haskell(x: float) -> str:
    """Format a float to match Haskell's show behavior.

    Haskell uses exponential notation for small numbers (abs < 0.1, except 0).
    Uses repr() for full round-trip precision in all cases.
    """
    import math
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "-Infinity" if x < 0 else "Infinity"
    if x == 0.0:
        return "-0.0" if math.copysign(1.0, x) < 0 else "0.0"

    abs_x = abs(x)

    if abs_x < 0.1:
        # Haskell uses exponential notation for small numbers
        r = repr(x)
        if 'e' in r or 'E' in r:
            # Already exponential; normalize Python's e-05 to Haskell's e-5
            r = r.replace('e-0', 'e-').replace('e+0', 'e+').replace('e+', 'e')
            parts = r.split('e')
            if '.' not in parts[0]:
                parts[0] += '.0'
            return 'e'.join(parts)
        else:
            # repr gave non-exponential (e.g. 0.05), convert to exponential
            import math
            exp = math.floor(math.log10(abs_x))
            mantissa = x / (10 ** exp)
            m_str = repr(mantissa)
            if '.' not in m_str:
                m_str += '.0'
            return f'{m_str}e{exp}'
    else:
        result = repr(x)
        if '.' not in result and 'e' not in result:
            result += '.0'
        return result


def bigfloat_to_bigint(x: Decimal) -> int:
    """Convert a bigfloat (Double) to a bigint (Integer)."""
    return round(x)


def bigfloat_to_float32(x: Decimal) -> float:
    """Convert a bigfloat (Double) to a float32 (Float)."""
    import struct
    # Pack as float32, unpack as float32 to get proper rounding
    f64 = float(x)
    f32_bytes = struct.pack('f', f64)
    f32 = struct.unpack('f', f32_bytes)[0]
    return f32


def bigfloat_to_float64(x: Decimal) -> float:
    """Convert a bigfloat (Double) to a float64 (Double)."""
    return float(x)


def bigint_to_bigfloat(x: int) -> Decimal:
    """Convert a bigint (Integer) to a bigfloat (Double)."""
    return Decimal(x)


def bigint_to_int8(x: int) -> int:
    """Convert a bigint (Integer) to an int8."""
    return x


def bigint_to_int16(x: int) -> int:
    """Convert a bigint (Integer) to an int16."""
    return x


def bigint_to_int32(x: int) -> int:
    """Convert a bigint (Integer) to an int32."""
    return x


def bigint_to_int64(x: int) -> int:
    """Convert a bigint (Integer) to an int64."""
    return x


def bigint_to_uint8(x: int) -> int:
    """Convert a bigint (Integer) to a uint8."""
    return x


def bigint_to_uint16(x: int) -> int:
    """Convert a bigint (Integer) to a uint16."""
    return x


def bigint_to_uint32(x: int) -> int:
    """Convert a bigint (Integer) to a uint32."""
    return x


def bigint_to_uint64(x: int) -> int:
    """Convert a bigint (Integer) to a uint64."""
    return x


def binary_to_bytes(s: bytes) -> tuple[int, ...]:
    """Convert binary to a list of byte values (0-255)."""
    return tuple(s)


def binary_to_string(s: bytes) -> str:
    """Convert binary to string by base64 encoding."""
    import base64
    return base64.b64encode(s).decode('ascii')


def float32_to_bigfloat(x: float) -> Decimal:
    """Convert a float32 (Float) to a bigfloat (Double)."""
    return Decimal(str(x))


def float64_to_bigfloat(x: float) -> Decimal:
    """Convert a float64 (Double) to a bigfloat (Double)."""
    return Decimal(str(x))


def int8_to_bigint(x: int) -> int:
    """Convert an int8 to a bigint (Integer)."""
    return x


def int16_to_bigint(x: int) -> int:
    """Convert an int16 to a bigint (Integer)."""
    return x


def int32_to_bigint(x: int) -> int:
    """Convert an int32 to a bigint (Integer)."""
    return x


def int64_to_bigint(x: int) -> int:
    """Convert an int64 to a bigint (Integer)."""
    return x


def read_bigfloat(s: str) -> Maybe[Decimal]:
    """Parse a string to a bigfloat (Double)."""
    try:
        return Just(Decimal(s))
    except:
        return NOTHING


def read_bigint(s: str) -> Maybe[int]:
    """Parse a string to a bigint (Integer)."""
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
    """Parse a string to a float32 (Float)."""
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
    """Parse a string to a float64 (Double)."""
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
    """Parse a string to an int32."""
    try:
        return Just(int(s))
    except:
        return NOTHING


def read_int64(s: str) -> Maybe[int]:
    """Parse a string to an int64."""
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
    """Convert a bigfloat (Double) to string."""
    return _format_float_like_haskell(float(x))


def show_bigint(x: int) -> str:
    """Convert a bigint (Integer) to string."""
    return str(x)


def show_boolean(b: bool) -> str:
    """Convert a boolean to string."""
    return "true" if b else "false"


def show_float32(x: float) -> str:
    """Convert a float32 (Float) to string."""
    import struct, math
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "-Infinity" if x < 0 else "Infinity"
    # Round-trip through float32 to get proper precision
    f32_bytes = struct.pack('f', x)
    f32 = struct.unpack('f', f32_bytes)[0]
    # Format with limited precision (6 significant digits for float32)
    # Use 'g' format which removes trailing zeros and uses exponential for small numbers
    if f32 == 0.0:
        return "0.0"
    abs_f32 = abs(f32)
    if abs_f32 < 0.1:
        # Exponential notation for small numbers
        formatted = f"{f32:.1e}"
        if 'e-0' in formatted:
            formatted = formatted.replace('e-0', 'e-')
        elif 'e+0' in formatted:
            formatted = formatted.replace('e+0', 'e')
        return formatted
    else:
        # Use float32's natural precision (about 6-7 digits)
        # Round to 6 significant figures
        from math import log10, floor
        magnitude = floor(log10(abs_f32))
        rounded = round(f32, -int(magnitude) + 5)  # 6 significant figures
        result = repr(rounded)
        if '.' not in result and 'e' not in result:
            result += '.0'
        return result


def show_float64(x: float) -> str:
    """Convert a float64 (Double) to string."""
    return _format_float_like_haskell(x)


def show_int8(x: int) -> str:
    """Convert an int8 to string."""
    return str(x)


def show_int16(x: int) -> str:
    """Convert an int16 to string."""
    return str(x)


def show_int32(x: int) -> str:
    """Convert an int32 to string."""
    return str(x)


def show_int64(x: int) -> str:
    """Convert an int64 to string."""
    return str(x)


def show_string(s: str) -> str:
    """Convert a string to a quoted string representation."""
    # ASCII control character names matching Haskell's show for Char
    _ASCII_CONTROL_NAMES = [
        "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "a",
        "b",   "t",   "n",   "v",   "f",   "r",   "SO",  "SI",
        "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
        "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
    ]
    sb = []
    last_was_numeric_escape = False
    for c in s:
        cp = ord(c)
        if last_was_numeric_escape and c.isdigit():
            sb.append("\\&")
        last_was_numeric_escape = False
        if c == '\\':
            sb.append("\\\\")
        elif c == '"':
            sb.append('\\"')
        elif cp < 0x20:
            sb.append('\\')
            sb.append(_ASCII_CONTROL_NAMES[cp])
        elif cp == 0x7F:
            sb.append("\\DEL")
        elif cp > 0x7F:
            sb.append('\\')
            sb.append(str(cp))
            last_was_numeric_escape = True
        else:
            sb.append(c)
    return '"' + ''.join(sb) + '"'


def show_uint8(x: int) -> str:
    """Convert a uint8 to string."""
    return str(x)


def show_uint16(x: int) -> str:
    """Convert a uint16 to string."""
    return str(x)


def show_uint32(x: int) -> str:
    """Convert a uint32 to string."""
    return str(x)


def show_uint64(x: int) -> str:
    """Convert a uint64 to string."""
    return str(x)


def string_to_binary(s: str) -> bytes:
    """Convert string to binary by base64 decoding."""
    import base64
    return base64.b64decode(s)


def uint8_to_bigint(x: int) -> int:
    """Convert a uint8 to a bigint (Integer)."""
    return x


def uint16_to_bigint(x: int) -> int:
    """Convert a uint16 to a bigint (Integer)."""
    return x


def uint32_to_bigint(x: int) -> int:
    """Convert a uint32 to a bigint (Integer)."""
    return x


def uint64_to_bigint(x: int) -> int:
    """Convert a uint64 to a bigint (Integer)."""
    return x
