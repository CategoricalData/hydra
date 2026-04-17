"""Phantom-typed term DSL for the hydra.lib.math library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import (
    lib_primitive as primitive,
    lib_primitive1 as primitive1,
    lib_primitive2 as primitive2,
)


def abs_(x: TTerm) -> TTerm:
    """Return the absolute value of a number."""
    return primitive1(x)


def acos(x: TTerm) -> TTerm:
    """Return the arc cosine of x in radians."""
    return primitive1(x)


def acosh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic cosine of x."""
    return primitive1(x)


def add(x: TTerm, y: TTerm) -> TTerm:
    """Add two numbers."""
    return primitive2(x, y)


def add_float64(x: TTerm, y: TTerm) -> TTerm:
    """Add two Float64 numbers."""
    return primitive2(x, y)


def asin(x: TTerm) -> TTerm:
    """Return the arc sine of x in radians."""
    return primitive1(x)


def asinh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic sine of x."""
    return primitive1(x)


def atan(x: TTerm) -> TTerm:
    """Return the arc tangent of x in radians."""
    return primitive1(x)


def atan2(y: TTerm, x: TTerm) -> TTerm:
    """Return the arc tangent of y/x in radians, using signs to determine quadrant."""
    return primitive2(y, x)


def atanh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic tangent of x."""
    return primitive1(x)


def ceiling(x: TTerm) -> TTerm:
    """Return the ceiling of x as a float64."""
    return primitive1(x)


def cos(x: TTerm) -> TTerm:
    """Return the cosine of x radians."""
    return primitive1(x)


def cosh(x: TTerm) -> TTerm:
    """Return the hyperbolic cosine of x."""
    return primitive1(x)


def e() -> TTerm:
    """Euler's number (e ≈ 2.71828)."""
    return primitive()


def even(x: TTerm) -> TTerm:
    """Check if an integer is even."""
    return primitive1(x)


def exp(x: TTerm) -> TTerm:
    """Return e raised to the power x."""
    return primitive1(x)


def floor(x: TTerm) -> TTerm:
    """Return the floor of x as a float64."""
    return primitive1(x)


def log(x: TTerm) -> TTerm:
    """Return the natural logarithm of x."""
    return primitive1(x)


def log_base(base: TTerm, x: TTerm) -> TTerm:
    """Return the logarithm of x to the given base."""
    return primitive2(base, x)


def max_(x: TTerm, y: TTerm) -> TTerm:
    """Return the maximum of two values."""
    return primitive2(x, y)


def maybe_div(x: TTerm, y: TTerm) -> TTerm:
    """Divide two integers using integer division, returning Nothing on division by zero."""
    return primitive2(x, y)


def maybe_mod(a: TTerm, b: TTerm) -> TTerm:
    """Mathematical modulo, returning Nothing on division by zero."""
    return primitive2(a, b)


def maybe_pred(x: TTerm) -> TTerm:
    """Return the predecessor (x - 1), returning Nothing on minBound."""
    return primitive1(x)


def maybe_rem(a: TTerm, b: TTerm) -> TTerm:
    """Integer remainder, returning Nothing on division by zero."""
    return primitive2(a, b)


def maybe_succ(x: TTerm) -> TTerm:
    """Return the successor (x + 1), returning Nothing on maxBound."""
    return primitive1(x)


def min_(x: TTerm, y: TTerm) -> TTerm:
    """Return the minimum of two values."""
    return primitive2(x, y)


def mul(x: TTerm, y: TTerm) -> TTerm:
    """Multiply two numbers."""
    return primitive2(x, y)


def mul_float64(x: TTerm, y: TTerm) -> TTerm:
    """Multiply two Float64 numbers."""
    return primitive2(x, y)


def negate(x: TTerm) -> TTerm:
    """Negate a number."""
    return primitive1(x)


def negate_float64(x: TTerm) -> TTerm:
    """Negate a Float64 number."""
    return primitive1(x)


def odd(x: TTerm) -> TTerm:
    """Check if an integer is odd."""
    return primitive1(x)


def pi() -> TTerm:
    """Pi (π ≈ 3.14159)."""
    return primitive()


def pow_(x: TTerm, y: TTerm) -> TTerm:
    """Return x raised to the power y."""
    return primitive2(x, y)


def range_(start: TTerm, end: TTerm) -> TTerm:
    """Generate a range of values from start to end (inclusive)."""
    return primitive2(start, end)


def round_(x: TTerm) -> TTerm:
    """Return x rounded to the nearest integer, as a float64."""
    return primitive1(x)


def round_bigfloat(digits: TTerm, x: TTerm) -> TTerm:
    """Round a bigfloat to n significant digits."""
    return primitive2(digits, x)


def round_float32(digits: TTerm, x: TTerm) -> TTerm:
    """Round a float32 to n significant digits."""
    return primitive2(digits, x)


def round_float64(digits: TTerm, x: TTerm) -> TTerm:
    """Round a float64 to n significant digits."""
    return primitive2(digits, x)


def signum(x: TTerm) -> TTerm:
    """Return the sign of a number (-1, 0, or 1)."""
    return primitive1(x)


def sin(x: TTerm) -> TTerm:
    """Return the sine of x radians."""
    return primitive1(x)


def sinh(x: TTerm) -> TTerm:
    """Return the hyperbolic sine of x."""
    return primitive1(x)


def sqrt(x: TTerm) -> TTerm:
    """Return the square root of x."""
    return primitive1(x)


def sub(x: TTerm, y: TTerm) -> TTerm:
    """Subtract two numbers."""
    return primitive2(x, y)


def sub_float64(x: TTerm, y: TTerm) -> TTerm:
    """Subtract two Float64 numbers."""
    return primitive2(x, y)


def tan(x: TTerm) -> TTerm:
    """Return the tangent of x radians."""
    return primitive1(x)


def tanh(x: TTerm) -> TTerm:
    """Return the hyperbolic tangent of x."""
    return primitive1(x)


def truncate(x: TTerm) -> TTerm:
    """Return x truncated (towards zero), as a float64."""
    return primitive1(x)
