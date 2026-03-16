"""Python implementations of hydra.lib.math primitives."""

from __future__ import annotations
import math
from decimal import Decimal

from hydra.dsl.python import frozenlist


import builtins as _builtins

def abs_(x: int) -> int:
    """Return the absolute value of an integer."""
    return _builtins.abs(x)

# Alias for Hydra compatibility (abs is a Python builtin)
abs = abs_


def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y


def div(x: int, y: int) -> int:
    """Divide two integers using integer division."""
    return x // y


def even(x: int) -> bool:
    """Check if an integer is even."""
    return x % 2 == 0


def max_(x: int, y: int) -> int:
    """Return the maximum of two integers."""
    return _builtins.max(x, y)

# Alias for Hydra compatibility (max is a Python builtin)
max = max_


def min_(x: int, y: int) -> int:
    """Return the minimum of two integers."""
    return _builtins.min(x, y)

# Alias for Hydra compatibility (min is a Python builtin)
min = min_


def mod(a: int, b: int) -> int:
    """Mathematical modulo with result having same sign as divisor."""
    return a % b


def mul(x: int, y: int) -> int:
    """Multiply two integers."""
    return x * y


def negate(x: int) -> int:
    """Negate an integer."""
    return -x


def odd(x: int) -> bool:
    """Check if an integer is odd."""
    return x % 2 != 0


def pred(x: int) -> int:
    """Return the predecessor of an integer (x - 1)."""
    return x - 1


def range_(start: int, end: int) -> frozenlist[int]:
    """Generate a range of integers from start to end (inclusive)."""
    return tuple(range(start, end + 1))


def rem(a: int, b: int) -> int:
    """Integer remainder with result having same sign as dividend (Haskell semantics).

    This uses truncated division, not floored division like Python's %.
    For example: rem(-10, 3) = -1, rem(10, -3) = 1
    """
    # Use int() to truncate toward zero (Haskell behavior)
    # Python's // floors toward negative infinity
    q = int(a / b)  # Truncate toward zero
    return a - q * b


def signum(x: int) -> int:
    """Return the sign of an integer (-1, 0, or 1)."""
    if x < 0:
        return -1
    elif x > 0:
        return 1
    else:
        return 0


def sub(x: int, y: int) -> int:
    """Subtract two integers."""
    return x - y


def succ(x: int) -> int:
    """Return the successor of an integer (x + 1)."""
    return x + 1


# Floating-point constants (defined as zero-arg functions for consistency with Hydra primitives)

def e() -> float:
    """Euler's number (e ≈ 2.71828)."""
    return math.e


def pi() -> float:
    """Pi (π ≈ 3.14159)."""
    return math.pi


# Trigonometric functions

def acos(x: float) -> float:
    """Return the arc cosine of x in radians."""
    return math.acos(x)


def asin(x: float) -> float:
    """Return the arc sine of x in radians."""
    return math.asin(x)


def atan(x: float) -> float:
    """Return the arc tangent of x in radians."""
    return math.atan(x)


def atan2(y: float, x: float) -> float:
    """Return the arc tangent of y/x in radians, using signs to determine quadrant."""
    return math.atan2(y, x)


def cos(x: float) -> float:
    """Return the cosine of x radians."""
    return math.cos(x)


def sin(x: float) -> float:
    """Return the sine of x radians."""
    return math.sin(x)


def tan(x: float) -> float:
    """Return the tangent of x radians."""
    return math.tan(x)


# Hyperbolic functions

def acosh(x: float) -> float:
    """Return the inverse hyperbolic cosine of x."""
    return math.acosh(x)


def asinh(x: float) -> float:
    """Return the inverse hyperbolic sine of x."""
    return math.asinh(x)


def atanh(x: float) -> float:
    """Return the inverse hyperbolic tangent of x."""
    return math.atanh(x)


def cosh(x: float) -> float:
    """Return the hyperbolic cosine of x."""
    return math.cosh(x)


def sinh(x: float) -> float:
    """Return the hyperbolic sine of x."""
    return math.sinh(x)


def tanh(x: float) -> float:
    """Return the hyperbolic tangent of x."""
    return math.tanh(x)


# Power and logarithmic functions

def exp(x: float) -> float:
    """Return e raised to the power x."""
    return math.exp(x)


def log(x: float) -> float:
    """Return the natural logarithm of x."""
    return math.log(x)


def log_base(base: float, x: float) -> float:
    """Return the logarithm of x to the given base."""
    return math.log(x, base)


def pow_(x: float, y: float) -> float:
    """Return x raised to the power y."""
    return math.pow(x, y)

# Alias for Hydra compatibility (pow is a Python builtin)
pow = pow_


def sqrt(x: float) -> float:
    """Return the square root of x."""
    return math.sqrt(x)


# Rounding functions

def ceiling(x: float) -> int:
    """Return the ceiling of x as an integer."""
    return math.ceil(x)


def floor(x: float) -> int:
    """Return the floor of x as an integer."""
    return math.floor(x)


def round_(x: float) -> int:
    """Return x rounded to the nearest integer."""
    return _builtins.round(x)

# Alias for Hydra compatibility (round is a Python builtin)
round = round_


def round_bigfloat(n: int, x: Decimal) -> Decimal:
    """Round a bigfloat (Decimal) to n significant digits.

    Converts through float to match Haskell semantics (where bigfloat is Double).
    Uses str() for the Decimal conversion to avoid float64 representation artifacts.
    """
    return Decimal(str(round_float64(n, float(x))))


def round_float32(n: int, x: float) -> float:
    """Round a float32 to n significant digits.

    Performs computation in float32 precision to match Haskell semantics.
    """
    import struct
    if x == 0:
        return 0.0
    # Round-trip through float32 to ensure input is in float32 precision
    x32 = struct.unpack('f', struct.pack('f', x))[0]
    factor = 10 ** (n - 1 - math.floor(math.log10(_builtins.abs(x32))))
    result = _builtins.round(x32 * factor) / factor
    # Round-trip result through float32
    return struct.unpack('f', struct.pack('f', result))[0]


def round_float64(n: int, x: float) -> float:
    """Round a float64 to n significant digits."""
    if x == 0:
        return 0.0
    factor = 10 ** (n - 1 - math.floor(math.log10(_builtins.abs(x))))
    return _builtins.round(x * factor) / factor


def truncate(x: float) -> int:
    """Return x truncated to an integer (towards zero)."""
    return math.trunc(x)
