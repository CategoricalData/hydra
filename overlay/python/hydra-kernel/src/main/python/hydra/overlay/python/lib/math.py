"""Python implementations of hydra.lib.math primitives."""

from __future__ import annotations
import math
from decimal import Decimal

from hydra.overlay.python.dsl.python import frozenlist, Optional, Given, None_


import builtins as _builtins


# ===== Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate =====
#
# These primitives are registered with a 'numeric' class constraint and identity (Term) coders
# (see sources/libraries.py), so on the interpreter path they receive raw Terms and must dispatch
# on the operand's literal variant. On the generated-code path they are called directly with native
# Python numbers. Both are handled in one function, dual-mode, mirroring equality.compare: try the
# native operation first and fall back to Term dispatch on TypeError. Python's arithmetic operators
# are already runtime-polymorphic, so the native path needs no per-type branching; the Term path
# reconstructs the same literal variant so the result stays correctly typed. No typeclass mechanism
# is consulted at runtime — the host has none.

def _numeric_binary(x, y, op):
    """Apply a binary numeric operation, dispatching Terms on their literal variant when needed."""
    try:
        return op(x, y)
    except TypeError:
        return _numeric_binary_terms(x, y, op)


def _numeric_unary(x, op):
    """Apply a unary numeric operation, dispatching a Term on its literal variant when needed."""
    try:
        return op(x)
    except TypeError:
        return _numeric_unary_terms(x, op)


def _numeric_binary_terms(x, y, op):
    import hydra.core
    lx = _numeric_literal(x)
    ly = _numeric_literal(y)
    if isinstance(lx, hydra.core.LiteralInteger) and isinstance(ly, hydra.core.LiteralInteger):
        return hydra.core.TermLiteral(hydra.core.LiteralInteger(
            _rewrap_integer(lx.value, op(lx.value.value, ly.value.value))))
    if isinstance(lx, hydra.core.LiteralFloat) and isinstance(ly, hydra.core.LiteralFloat):
        return hydra.core.TermLiteral(hydra.core.LiteralFloat(
            _rewrap_float(lx.value, op(lx.value.value, ly.value.value))))
    raise TypeError("hydra.lib.math: operands are not the same numeric kind")


def _numeric_unary_terms(x, op):
    import hydra.core
    lx = _numeric_literal(x)
    if isinstance(lx, hydra.core.LiteralInteger):
        return hydra.core.TermLiteral(hydra.core.LiteralInteger(
            _rewrap_integer(lx.value, op(lx.value.value))))
    if isinstance(lx, hydra.core.LiteralFloat):
        return hydra.core.TermLiteral(hydra.core.LiteralFloat(
            _rewrap_float(lx.value, op(lx.value.value))))
    raise TypeError("hydra.lib.math: operand is not numeric")


def _numeric_literal(term):
    import hydra.core
    if isinstance(term, hydra.core.TermLiteral):
        return term.value
    raise TypeError("hydra.lib.math: expected a literal term")


# Width (signed?, bits) per fixed-width IntegerValue variant, keyed by class NAME to avoid importing
# hydra.core at module-load time. Bigint has no entry — it keeps full arbitrary precision.
_INT_WIDTHS = {
    "IntegerValueInt8": (True, 8),
    "IntegerValueInt16": (True, 16),
    "IntegerValueInt32": (True, 32),
    "IntegerValueInt64": (True, 64),
    "IntegerValueUint8": (False, 8),
    "IntegerValueUint16": (False, 16),
    "IntegerValueUint32": (False, 32),
    "IntegerValueUint64": (False, 64),
}


def _rewrap_integer(like, value):
    """Reconstruct the same IntegerValue variant as `like`, narrowing the result to the variant's
    width. Fixed-width integers get two's-complement wraparound (matching the Haskell and Java
    hosts, whose fixed-width representation types wrap on overflow); bigint keeps full precision.
    Python ints are arbitrary-precision, so without this narrowing an int32 overflow would silently
    produce an out-of-range value and diverge from the other hosts."""
    width = _INT_WIDTHS.get(type(like).__name__)
    if width is not None:
        signed, bits = width
        value &= (1 << bits) - 1
        if signed and value >= (1 << (bits - 1)):
            value -= (1 << bits)
    return type(like)(value)


def _rewrap_float(like, value):
    """Reconstruct the same FloatValue variant as `like` around a new native float value."""
    return type(like)(value)


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


def add(x, y):
    """Add two numbers (constraint-polymorphic over the 'numeric' class)."""
    return _numeric_binary(x, y, lambda a, b: a + b)


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


def div(x: int, y: int) -> Optional[int]:
    """Divide two integers, returning none if the divisor is zero."""
    return None_() if y == 0 else Given(x // y)


def mod(x: int, y: int) -> Optional[int]:
    """Mathematical modulo, returning none if the divisor is zero."""
    return None_() if y == 0 else Given(x % y)


def maybe_pred(x: int) -> Optional[int]:
    """Return the predecessor, returning none if x is minBound."""
    return None_() if x == -2147483648 else Given(x - 1)


def rem(x: int, y: int) -> Optional[int]:
    """Integer remainder, returning none if the divisor is zero."""
    if y == 0:
        return None_()
    q = int(x / y)
    return Given(x - q * y)


def maybe_succ(x: int) -> Optional[int]:
    """Return the successor, returning none if x is maxBound."""
    return None_() if x == 2147483647 else Given(x + 1)


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


def mul(x, y):
    """Multiply two numbers (constraint-polymorphic over the 'numeric' class)."""
    return _numeric_binary(x, y, lambda a, b: a * b)


def mul_float64(x: float, y: float) -> float:
    """Multiply two Float64 numbers."""
    return x * y


def negate(x):
    """Negate a number (constraint-polymorphic over the 'numeric' class)."""
    return _numeric_unary(x, lambda a: -a)


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


def round_float32(n: int, x: float) -> float:
    """Round a float32 to n significant digits."""
    import struct
    if x == 0:
        return 0.0
    if math.isnan(x) or math.isinf(x):
        return x
    # Round-trip through float32 to ensure input is in float32 precision.
    # If the input underflows float32 (e.g. very small denormals), x32 may
    # be 0.0 even though x != 0; treat it as zero rather than passing 0.0
    # to log10 (which would give -inf and crash math.floor → OverflowError).
    x32 = struct.unpack('f', struct.pack('f', x))[0]
    if x32 == 0:
        return 0.0
    if math.isnan(x32) or math.isinf(x32):
        return x32
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


def sub(x, y):
    """Subtract two numbers (constraint-polymorphic over the 'numeric' class)."""
    return _numeric_binary(x, y, lambda a, b: a - b)


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
