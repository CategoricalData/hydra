"""Python implementations of hydra.lib.literals primitives."""

from __future__ import annotations
from decimal import Decimal
from hydra.overlay.python.dsl.python import Optional, Given, NONE_


def _shortest_round_trip_digits(x: float, pack_fmt: str) -> str:
    """Return the shortest %g-style digit string that round-trips to the same bit pattern as x
    under the given struct format ('d' for float64, 'f' for float32). For float64, repr(x)
    already gives this (Python's float repr is defined as shortest-round-trip), so this is only
    exercised for float32 -- repr(x) on a float32-narrowed Python float can carry up to 17
    significant digits (full double precision) when only up to 9 are needed to round-trip at
    float32 precision, e.g. 281474943156225.0 round-trips at float32 as "2.8147494e14", not
    "2.81474943156225e14". Near FLT_MAX, rounding x to a low significant-digit count can round
    UP past FLT_MAX (e.g. 3.4028234663852886e38 rounds to '3.403e+38' at 4 significant figures,
    which exceeds FLT_MAX) -- struct.pack raises OverflowError for such a candidate, which just
    means it doesn't round-trip, so the search should continue rather than propagate."""
    import struct
    target = struct.pack(pack_fmt, x)
    max_sig = 9 if pack_fmt == 'f' else 17
    for sig in range(1, max_sig):
        s = f'{x:.{sig}g}'
        try:
            if struct.pack(pack_fmt, float(s)) == target:
                return s
        except OverflowError:
            continue
    return f'{x:.{max_sig}g}'


def _digits_and_exponent(g: str) -> tuple[bool, str, int]:
    """Decompose a %g-style digit string (decimal or exponential, e.g. '1e+01', '16777216',
    '0.05') into (is_negative, significant_digits, decimal_exponent), where the value equals
    +/-0.d1d2...dN * 10^(exponent+1) -- i.e. exponent is the power of ten of the leading digit.
    Trailing/leading zeros are stripped from the digit string (a bare '0' if the value is zero)."""
    neg = g.startswith('-')
    gg = g[1:] if neg else g
    mantissa, _, exp_str = gg.partition('e')
    exp = int(exp_str) if exp_str else 0
    int_part, _, frac_part = mantissa.partition('.')
    if int_part.lstrip('0'):
        exponent = exp + len(int_part) - 1
        digits = (int_part + frac_part).lstrip('0')
    else:
        stripped = frac_part.lstrip('0')
        exponent = exp - (len(frac_part) - len(stripped) + 1)
        digits = stripped
    digits = digits.rstrip('0') or '0'
    return neg, digits, exponent


def _format_float_like_haskell(x: float, pack_fmt: str = 'd') -> str:
    """Format a float to match Haskell's `show` behavior for RealFloat: decimal notation for
    0.1 <= abs(x) < 10^7, exponential notation outside that range (verified empirically against
    GHC: `show (16777216.0 :: Double)` = "1.6777216e7", not "16777216.0"). `pack_fmt` selects the
    round-trip precision to target ('d' for float64, 'f' for float32) -- both bounds matter:
    this function's absence of an upper bound (fixed here) was a real, previously-untriggered
    bug, only caught by #317's land-gate bootstrap (the first test suite run to exercise a
    float64 value >= 10^7). Always decomposes the shortest-round-trip digit string into
    (sign, digits, exponent) via _digits_and_exponent before choosing notation -- Python's %g
    can return exponential form (e.g. '1e+01' for 10.0 at 1 significant digit) even when
    Haskell's notation rule calls for decimal, so the digit string can't be used verbatim."""
    import math
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "-Infinity" if x < 0 else "Infinity"
    if x == 0.0:
        return "-0.0" if math.copysign(1.0, x) < 0 else "0.0"

    abs_x = abs(x)
    g = _shortest_round_trip_digits(x, pack_fmt)
    neg, digits, exponent = _digits_and_exponent(g)
    sign = "-" if neg else ""

    if abs_x < 0.1 or abs_x >= 1e7:
        # Haskell uses exponential notation outside [0.1, 10^7)
        mantissa = digits[0] + '.' + (digits[1:] or '0')
        return f'{sign}{mantissa}e{exponent}'
    else:
        # Decimal notation
        if exponent >= 0:
            int_part = digits[:exponent + 1].ljust(exponent + 1, '0')
            frac_part = digits[exponent + 1:] or '0'
        else:
            int_part = '0'
            frac_part = '0' * (-exponent - 1) + digits
        return f'{sign}{int_part}.{frac_part}'


def bigint_to_decimal(x: int) -> Decimal:
    """Convert a bigint (Integer) to a decimal (arbitrary-precision exact decimal)."""
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


def binary_to_base64(s: bytes) -> str:
    """Convert binary to string by base64 encoding."""
    import base64
    return base64.b64encode(s).decode('ascii')


def decimal_to_bigint(x: Decimal) -> int:
    """Convert a decimal to a bigint using banker's rounding (round half to even)."""
    import decimal as _d
    return int(x.to_integral_value(rounding=_d.ROUND_HALF_EVEN))


def decimal_to_float32(x: Decimal) -> float:
    """Convert a decimal to a float32 (Float), with possible loss of precision."""
    import struct
    f64 = float(x)
    f32_bytes = struct.pack('f', f64)
    f32 = struct.unpack('f', f32_bytes)[0]
    return f32


def decimal_to_float64(x: Decimal) -> float:
    """Convert a decimal to a float64 (Double), with possible loss of precision."""
    return float(x)


def float32_to_decimal(x: float) -> Decimal:
    """Convert a float32 (Float) to a decimal."""
    return Decimal(str(x))


def float32_to_float64(x: float) -> float:
    """Convert a float32 to a float64 (lossless widening; Python floats are 64-bit, so this is identity)."""
    return x


def float64_to_decimal(x: float) -> Decimal:
    """Convert a float64 (Double) to a decimal."""
    return Decimal(str(x))


def float64_to_float32(x: float) -> float:
    """Convert a float64 to a float32 (lossy narrowing). Round-trips through 4-byte IEEE 754 single."""
    import struct
    return struct.unpack('f', struct.pack('f', x))[0]


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


def parse_bigint(s: str) -> Optional[int]:
    """Parse a string to a bigint (Integer)."""
    try:
        return Given(int(s))
    except:
        return NONE_


def parse_boolean(s: str) -> Optional[bool]:
    """Parse a string to a boolean."""
    if s == "true":
        return Given(True)
    elif s == "false":
        return Given(False)
    else:
        return NONE_


def parse_decimal(s: str) -> Optional[Decimal]:
    """Parse a string to a decimal (arbitrary-precision exact decimal)."""
    try:
        return Given(Decimal(s))
    except:
        return NONE_


def _parse_float_token(s: str) -> float:
    """Parse a float string accepting Hydra special tokens
    (Infinity, -Infinity, NaN) as well as regular floats. Python's
    float() accepts 'inf'/'nan' but not Hydra's canonical 'Infinity'/'NaN'."""
    if s == "Infinity":
        return float("inf")
    if s == "-Infinity":
        return float("-inf")
    if s == "NaN":
        return float("nan")
    return float(s)


def parse_float32(s: str) -> Optional[float]:
    """Parse a string to a float32 (Float)."""
    import struct
    try:
        f64 = _parse_float_token(s)
        # Convert to float32 precision
        f32_bytes = struct.pack('f', f64)
        f32 = struct.unpack('f', f32_bytes)[0]
        return Given(f32)
    except:
        return NONE_


def parse_float64(s: str) -> Optional[float]:
    """Parse a string to a float64 (Double)."""
    try:
        return Given(_parse_float_token(s))
    except:
        return NONE_


def parse_int8(s: str) -> Optional[int]:
    """Parse a string to an int8 (-128 to 127)."""
    try:
        n = int(s)
        if n >= -128 and n <= 127:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def parse_int16(s: str) -> Optional[int]:
    """Parse a string to an int16 (-32768 to 32767)."""
    try:
        n = int(s)
        if n >= -32768 and n <= 32767:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def parse_int32(s: str) -> Optional[int]:
    """Parse a string to an int32."""
    try:
        return Given(int(s))
    except:
        return NONE_


def parse_int64(s: str) -> Optional[int]:
    """Parse a string to an int64."""
    try:
        return Given(int(s))
    except:
        return NONE_


def parse_string(s: str) -> Optional[str]:
    """Parse a string literal."""
    try:
        import ast
        result = ast.literal_eval(s)
        return Given(result) if isinstance(result, str) else NONE_
    except:
        return NONE_


def parse_uint8(s: str) -> Optional[int]:
    """Parse a string to a uint8 (0 to 255)."""
    try:
        n = int(s)
        if n >= 0 and n <= 255:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def parse_uint16(s: str) -> Optional[int]:
    """Parse a string to a uint16 (0 to 65535)."""
    try:
        n = int(s)
        if n >= 0 and n <= 65535:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def parse_uint32(s: str) -> Optional[int]:
    """Parse a string to a uint32 (0 to 4294967295)."""
    try:
        n = int(s)
        if n >= 0 and n <= 4294967295:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def parse_uint64(s: str) -> Optional[int]:
    """Parse a string to a uint64 (0 to 18446744073709551615)."""
    try:
        n = int(s)
        if n >= 0 and n <= 18446744073709551615:
            return Given(n)
        else:
            return NONE_
    except:
        return NONE_


def print_bigint(x: int) -> str:
    """Convert a bigint (Integer) to string."""
    return str(x)


def print_boolean(b: bool) -> str:
    """Convert a boolean to string."""
    return "true" if b else "false"


def print_decimal(x: Decimal) -> str:
    """Convert a decimal to its string, matching Haskell's Data.Scientific show."""
    # Normalize: strip trailing zeros, then format as decimal or scientific.
    normalized = x.normalize() if x != 0 else Decimal(0)
    # decimal.Decimal.as_tuple() gives (sign, digits, exponent)
    sign, digits, exp = normalized.as_tuple()
    if not isinstance(exp, int):
        return str(x)
    coefficient = ''.join(str(d) for d in digits) or '0'
    # Haskell Scientific e = precision - scale - 1 where precision = len(digits),
    # scale = -exp. So e = len(digits) + exp - 1.
    if x == 0:
        return "0.0"
    e = len(digits) + exp - 1
    sign_str = "-" if sign else ""
    # Haskell Scientific uses plain form iff -1 <= e <= 6; otherwise scientific.
    if e >= 7 or e < -1:
        if len(coefficient) == 1:
            mantissa = coefficient + ".0"
        else:
            mantissa = coefficient[0] + "." + coefficient[1:]
        return f"{sign_str}{mantissa}e{e}"
    else:
        # Decimal notation with at least one digit after the dot
        if exp >= 0:
            plain = coefficient + "0" * exp + ".0"
        elif -exp < len(coefficient):
            idx = len(coefficient) + exp
            plain = coefficient[:idx] + "." + coefficient[idx:]
        else:
            plain = "0." + "0" * (-exp - len(coefficient)) + coefficient
        return f"{sign_str}{plain}"


def print_float32(x: float) -> str:
    """Convert a float32 (Float) to string, matching Haskell's show behavior.

    Three bugs fixed here (found via the #317 land-gate's python-to-haskell bootstrap path,
    which is the only path that actually exercises this Python-hosted Haskell-literal-printing
    logic, first run to touch a float32 magnitude >= 10^7, a -0.0/+0.0 case, or an exponential-
    notation value here): (1) the zero case collapsed BOTH +0.0 and -0.0 to the string "0.0",
    losing the sign bit; (2) the general case hand-rounded to a hardcoded 6 significant figures,
    but float32's shortest-round-trip representation needs up to 9 (e.g. 16777201.0, not a
    truncated 16777200.0); (3) the exponential-notation mantissa was computed at full float64
    precision (e.g. "2.81474943156224e14"), rather than the shortest string that round-trips at
    float32 precision (the correct "2.8147494e14", verified against GHC's `show :: Float`).
    Narrows to float32 precision via struct round-trip, then delegates to
    _format_float_like_haskell (shared with float64) for notation rules, passing pack_fmt='f'
    so the round-trip search targets float32 precision rather than float64.
    """
    import struct, math
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "-Infinity" if x < 0 else "Infinity"
    f32_bytes = struct.pack('f', x)
    f32 = struct.unpack('f', f32_bytes)[0]
    return _format_float_like_haskell(f32, pack_fmt='f')


def print_float64(x: float) -> str:
    """Convert a float64 (Double) to string."""
    return _format_float_like_haskell(x, pack_fmt='d')


def print_int8(x: int) -> str:
    """Convert an int8 to string."""
    return str(x)


def print_int16(x: int) -> str:
    """Convert an int16 to string."""
    return str(x)


def print_int32(x: int) -> str:
    """Convert an int32 to string."""
    return str(x)


def print_int64(x: int) -> str:
    """Convert an int64 to string."""
    return str(x)


def print_string(s: str) -> str:
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


def print_uint8(x: int) -> str:
    """Convert a uint8 to string."""
    return str(x)


def print_uint16(x: int) -> str:
    """Convert a uint16 to string."""
    return str(x)


def print_uint32(x: int) -> str:
    """Convert a uint32 to string."""
    return str(x)


def print_uint64(x: int) -> str:
    """Convert a uint64 to string."""
    return str(x)


def base64_to_binary(s: str) -> bytes:
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
