"""Phantom-typed term DSL for the hydra.lib.math library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import (
    lib_primitive as primitive,
    lib_primitive1 as primitive1,
    lib_primitive2 as primitive2,
)


def abs_(x: TypedTerm) -> TypedTerm:
    """Return the absolute value of a number."""
    return primitive1(x)


def acos(x: TypedTerm) -> TypedTerm:
    """Return the arc cosine of x in radians."""
    return primitive1(x)


def acosh(x: TypedTerm) -> TypedTerm:
    """Return the inverse hyperbolic cosine of x."""
    return primitive1(x)


def add(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Add two numbers."""
    return primitive2(x, y)


def add_float64(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Add two Float64 numbers."""
    return primitive2(x, y)


def asin(x: TypedTerm) -> TypedTerm:
    """Return the arc sine of x in radians."""
    return primitive1(x)


def asinh(x: TypedTerm) -> TypedTerm:
    """Return the inverse hyperbolic sine of x."""
    return primitive1(x)


def atan(x: TypedTerm) -> TypedTerm:
    """Return the arc tangent of x in radians."""
    return primitive1(x)


def atan2(y: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Return the arc tangent of y/x in radians, using signs to determine quadrant."""
    return primitive2(y, x)


def atanh(x: TypedTerm) -> TypedTerm:
    """Return the inverse hyperbolic tangent of x."""
    return primitive1(x)


def ceiling(x: TypedTerm) -> TypedTerm:
    """Return the ceiling of x as a float64."""
    return primitive1(x)


def cos(x: TypedTerm) -> TypedTerm:
    """Return the cosine of x radians."""
    return primitive1(x)


def cosh(x: TypedTerm) -> TypedTerm:
    """Return the hyperbolic cosine of x."""
    return primitive1(x)


def e() -> TypedTerm:
    """Euler's number (e ≈ 2.71828)."""
    return primitive()


def even(x: TypedTerm) -> TypedTerm:
    """Check if an integer is even."""
    return primitive1(x)


def exp(x: TypedTerm) -> TypedTerm:
    """Return e raised to the power x."""
    return primitive1(x)


def floor(x: TypedTerm) -> TypedTerm:
    """Return the floor of x as a float64."""
    return primitive1(x)


def log(x: TypedTerm) -> TypedTerm:
    """Return the natural logarithm of x."""
    return primitive1(x)


def log_base(base: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Return the logarithm of x to the given base."""
    return primitive2(base, x)


def max_(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Return the maximum of two values."""
    return primitive2(x, y)


def maybe_div(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Divide two integers using integer division, returning none on division by zero."""
    return primitive2(x, y)


def maybe_mod(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Mathematical modulo, returning none on division by zero."""
    return primitive2(a, b)


def maybe_pred(x: TypedTerm) -> TypedTerm:
    """Return the predecessor (x - 1), returning none on minBound."""
    return primitive1(x)


def maybe_rem(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Integer remainder, returning none on division by zero."""
    return primitive2(a, b)


def maybe_succ(x: TypedTerm) -> TypedTerm:
    """Return the successor (x + 1), returning none on maxBound."""
    return primitive1(x)


def min_(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Return the minimum of two values."""
    return primitive2(x, y)


def mul(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Multiply two numbers."""
    return primitive2(x, y)


def mul_float64(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Multiply two Float64 numbers."""
    return primitive2(x, y)


def negate(x: TypedTerm) -> TypedTerm:
    """Negate a number."""
    return primitive1(x)


def negate_float64(x: TypedTerm) -> TypedTerm:
    """Negate a Float64 number."""
    return primitive1(x)


def odd(x: TypedTerm) -> TypedTerm:
    """Check if an integer is odd."""
    return primitive1(x)


def pi() -> TypedTerm:
    """Pi (π ≈ 3.14159)."""
    return primitive()


def pow_(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Return x raised to the power y."""
    return primitive2(x, y)


def range_(start: TypedTerm, end: TypedTerm) -> TypedTerm:
    """Generate a range of values from start to end (inclusive)."""
    return primitive2(start, end)


def round_(x: TypedTerm) -> TypedTerm:
    """Return x rounded to the nearest integer, as a float64."""
    return primitive1(x)


def round_float32(digits: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Round a float32 to n significant digits."""
    return primitive2(digits, x)


def round_float64(digits: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Round a float64 to n significant digits."""
    return primitive2(digits, x)


def signum(x: TypedTerm) -> TypedTerm:
    """Return the sign of a number (-1, 0, or 1)."""
    return primitive1(x)


def sin(x: TypedTerm) -> TypedTerm:
    """Return the sine of x radians."""
    return primitive1(x)


def sinh(x: TypedTerm) -> TypedTerm:
    """Return the hyperbolic sine of x."""
    return primitive1(x)


def sqrt(x: TypedTerm) -> TypedTerm:
    """Return the square root of x."""
    return primitive1(x)


def sub(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Subtract two numbers."""
    return primitive2(x, y)


def sub_float64(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Subtract two Float64 numbers."""
    return primitive2(x, y)


def tan(x: TypedTerm) -> TypedTerm:
    """Return the tangent of x radians."""
    return primitive1(x)


def tanh(x: TypedTerm) -> TypedTerm:
    """Return the hyperbolic tangent of x."""
    return primitive1(x)


def truncate(x: TypedTerm) -> TypedTerm:
    """Return x truncated (towards zero), as a float64."""
    return primitive1(x)
