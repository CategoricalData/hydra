"""Python implementations of hydra.lib.math primitives."""

from __future__ import annotations
import math
from decimal import Decimal

from hydra.dsl.python import frozenlist, Maybe, Just, Nothing


import builtins as _builtins

def _safe(f, x):
    """Wrap a math function to return NaN/Inf on domain/range errors (matching Haskell/Java IEEE 754 behavior).
    Python's math module raises ValueError for e.g. sin(inf), cos(inf), log(-1),
    and OverflowError for e.g. exp(1e308), whereas Haskell and Java return NaN or Inf per IEEE 754."""
    try:
        return f(x)
    except ValueError:
        return float('nan')
    except OverflowError:
        return float('inf')

def abs_(x: int) -> int:
    """Return the absolute value."""
    return _builtins.abs(x)

# Alias for Hydra compatibility (abs is a Python builtin)
abs = abs_


def acos(x: float) -> float:
    """Return the arc cosine of x in radians."""
    return _safe(math.acos, x)


def acosh(x: float) -> float:
    """Return the inverse hyperbolic cosine of x."""
    return _safe(math.acosh, x)


def add(x: int, y: int) -> int:
    """Add two numbers."""
    return x + y


def add_float64(x: float, y: float) -> float:
    """Add two Float64 numbers."""
    return x + y


def asin(x: float) -> float:
    """Return the arc sine of x in radians."""
    return _safe(math.asin, x)


def asinh(x: float) -> float:
    """Return the inverse hyperbolic sine of x."""
    return math.asinh(x)


def atan(x: float) -> float:
    """Return the arc tangent of x in radians."""
    return math.atan(x)


def atan2(y: float, x: float) -> float:
    """Return the arc tangent of y/x in radians, using signs to determine quadrant.
    Matches Haskell behavior: returns NaN when both arguments are infinite."""
    if math.isinf(y) and math.isinf(x):
        return float('nan')
    return math.atan2(y, x)


def atanh(x: float) -> float:
    """Return the inverse hyperbolic tangent of x."""
    if x == 1.0:
        return float('inf')
    if x == -1.0:
        return float('-inf')
    return _safe(math.atanh, x)


def ceiling(x: float) -> float:
    """Return the ceiling of x as a float.
    Returns NaN/Inf for NaN/Inf inputs, matching Haskell/Java IEEE 754 behavior."""
    if math.isnan(x) or math.isinf(x):
        return x
    return float(math.ceil(x))


def cos(x: float) -> float:
    """Return the cosine of x radians."""
    return _safe(math.cos, x)


def cosh(x: float) -> float:
    """Return the hyperbolic cosine of x."""
    return math.cosh(x)


def e() -> float:
    """Euler's number (e ≈ 2.71828)."""
    return math.e


def even(x: int) -> bool:
    """Check if an integer is even."""
    return x % 2 == 0


def exp(x: float) -> float:
    """Return e raised to the power x."""
    return _safe(math.exp, x)


def floor(x: float) -> float:
    """Return the floor of x as a float.
    Returns NaN/Inf for NaN/Inf inputs, matching Haskell/Java IEEE 754 behavior."""
    if math.isnan(x) or math.isinf(x):
        return x
    return float(math.floor(x))


def log(x: float) -> float:
    """Return the natural logarithm of x."""
    if x == 0.0:
        return float('-inf')
    return _safe(math.log, x)


def log_base(base: float, x: float) -> float:
    """Return the logarithm of x to the given base."""
    if x == 0.0:
        return float('-inf')
    try:
        return math.log(x, base)
    except (ValueError, ZeroDivisionError):
        return float('nan')


def maybe_div(x: int, y: int) -> Maybe[int]:
    """Divide two integers, returning Nothing if the divisor is zero."""
    return Nothing() if y == 0 else Just(x // y)


def maybe_mod(x: int, y: int) -> Maybe[int]:
    """Mathematical modulo, returning Nothing if the divisor is zero."""
    return Nothing() if y == 0 else Just(x % y)


def maybe_pred(x: int) -> Maybe[int]:
    """Return the predecessor, returning Nothing if x is minBound."""
    return Nothing() if x == -2147483648 else Just(x - 1)


def maybe_rem(x: int, y: int) -> Maybe[int]:
    """Integer remainder, returning Nothing if the divisor is zero."""
    if y == 0:
        return Nothing()
    q = int(x / y)
    return Just(x - q * y)


def maybe_succ(x: int) -> Maybe[int]:
    """Return the successor, returning Nothing if x is maxBound."""
    return Nothing() if x == 2147483647 else Just(x + 1)


def max_(x: int, y: int) -> int:
    """Return the maximum of two values."""
    return _builtins.max(x, y)

# Alias for Hydra compatibility (max is a Python builtin)
max = max_


def min_(x: int, y: int) -> int:
    """Return the minimum of two values."""
    return _builtins.min(x, y)

# Alias for Hydra compatibility (min is a Python builtin)
min = min_


def mul(x: int, y: int) -> int:
    """Multiply two numbers."""
    return x * y


def mul_float64(x: float, y: float) -> float:
    """Multiply two Float64 numbers."""
    return x * y


def negate(x: int) -> int:
    """Negate a number."""
    return -x


def negate_float64(x: float) -> float:
    """Negate a Float64 number."""
    return -x


def odd(x: int) -> bool:
    """Check if an integer is odd."""
    return x % 2 != 0


def pi() -> float:
    """Pi (π ≈ 3.14159)."""
    return math.pi


def pow_(x: float, y: float) -> float:
    """Return x raised to the power y.
    Matches Java Math.pow / Haskell (**) IEEE 754 behavior."""
    try:
        return math.pow(x, y)
    except OverflowError:
        return float('-inf') if x < 0 and y % 2 == 1 else float('inf')
    except ValueError:
        # pow(0, negative) = Infinity; pow(negative, non-integer) = NaN
        if x == 0.0 and y < 0:
            return float('inf')
        return float('nan')

# Alias for Hydra compatibility (pow is a Python builtin)
pow = pow_


def range_(start: int, end: int) -> frozenlist[int]:
    """Generate a range of values from start to end (inclusive)."""
    return tuple(range(start, end + 1))


def round_(x: float) -> float:
    """Return x rounded to the nearest integer as a float.
    Returns NaN/Inf for NaN/Inf inputs, matching Haskell/Java IEEE 754 behavior."""
    if math.isnan(x) or math.isinf(x):
        return x
    return float(_builtins.round(x))

# Alias for Hydra compatibility (round is a Python builtin)
round = round_


def round_bigfloat(n: int, x: Decimal) -> Decimal:
    """Round a bigfloat to n significant digits."""
    return Decimal(str(round_float64(n, float(x))))


def round_float32(n: int, x: float) -> float:
    """Round a float32 to n significant digits."""
    import struct
    if x == 0:
        return 0.0
    if math.isnan(x) or math.isinf(x):
        return x
    # Round-trip through float32 to ensure input is in float32 precision
    x32 = struct.unpack('f', struct.pack('f', x))[0]
    factor = 10 ** (n - 1 - math.floor(math.log10(_builtins.abs(x32))))
    result = _builtins.round(x32 * factor) / factor
    # Round-trip result through float32
    return struct.unpack('f', struct.pack('f', result))[0]


def round_float64(n: int, x: float) -> float:
    """Round a float64 to n significant digits."""
    if math.isnan(x) or math.isinf(x):
        return x
    if x == 0:
        return 0.0
    factor = 10 ** (n - 1 - math.floor(math.log10(_builtins.abs(x))))
    return _builtins.round(x * factor) / factor


def signum(x: int) -> int:
    """Return the sign of a number (-1, 0, or 1)."""
    if x < 0:
        return -1
    elif x > 0:
        return 1
    else:
        return 0


def sin(x: float) -> float:
    """Return the sine of x radians."""
    return _safe(math.sin, x)


def sinh(x: float) -> float:
    """Return the hyperbolic sine of x."""
    return math.sinh(x)


def sqrt(x: float) -> float:
    """Return the square root of x."""
    return _safe(math.sqrt, x)


def sub(x: int, y: int) -> int:
    """Subtract two numbers."""
    return x - y


def sub_float64(x: float, y: float) -> float:
    """Subtract two Float64 numbers."""
    return x - y


def tan(x: float) -> float:
    """Return the tangent of x radians."""
    return _safe(math.tan, x)


def tanh(x: float) -> float:
    """Return the hyperbolic tangent of x."""
    return math.tanh(x)


def truncate(x: float) -> float:
    """Return x truncated to an integer as a float (towards zero).
    Returns NaN/Inf for NaN/Inf inputs, matching Haskell/Java IEEE 754 behavior."""
    if math.isnan(x) or math.isinf(x):
        return x
    return float(math.trunc(x))
