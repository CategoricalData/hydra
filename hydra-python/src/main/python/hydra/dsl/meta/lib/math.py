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


def add(x: TTerm, y: TTerm) -> TTerm:
    """Add two numbers."""
    return primitive2(x, y)


def div(x: TTerm, y: TTerm) -> TTerm:
    """Divide two numbers using integer division."""
    return primitive2(x, y)


def even(x: TTerm) -> TTerm:
    """Check if a number is even."""
    return primitive1(x)


def mod(a: TTerm, b: TTerm) -> TTerm:
    """Mathematical modulo operation."""
    return primitive2(a, b)


def mul(x: TTerm, y: TTerm) -> TTerm:
    """Multiply two numbers."""
    return primitive2(x, y)


def negate(x: TTerm) -> TTerm:
    """Negate a number."""
    return primitive1(x)


def odd(x: TTerm) -> TTerm:
    """Check if a number is odd."""
    return primitive1(x)


def pred(x: TTerm) -> TTerm:
    """Return the predecessor of a number (x - 1)."""
    return primitive1(x)


def range_(start: TTerm, end: TTerm) -> TTerm:
    """Generate a range of integers from start to end (inclusive)."""
    return primitive2(start, end)


def rem(a: TTerm, b: TTerm) -> TTerm:
    """Integer remainder operation."""
    return primitive2(a, b)


def signum(x: TTerm) -> TTerm:
    """Return the sign of a number (-1, 0, or 1)."""
    return primitive1(x)


def sub(x: TTerm, y: TTerm) -> TTerm:
    """Subtract two numbers."""
    return primitive2(x, y)


def succ(x: TTerm) -> TTerm:
    """Return the successor of a number (x + 1)."""
    return primitive1(x)


# Floating-point constants

def e() -> TTerm:
    """Euler's number (e ≈ 2.71828)."""
    return primitive()


def pi() -> TTerm:
    """Pi (π ≈ 3.14159)."""
    return primitive()


# Trigonometric functions

def acos(x: TTerm) -> TTerm:
    """Return the arc cosine of x in radians."""
    return primitive1(x)


def asin(x: TTerm) -> TTerm:
    """Return the arc sine of x in radians."""
    return primitive1(x)


def atan(x: TTerm) -> TTerm:
    """Return the arc tangent of x in radians."""
    return primitive1(x)


def atan2(y: TTerm, x: TTerm) -> TTerm:
    """Return the arc tangent of y/x in radians."""
    return primitive2(y, x)


def cos(x: TTerm) -> TTerm:
    """Return the cosine of x radians."""
    return primitive1(x)


def sin(x: TTerm) -> TTerm:
    """Return the sine of x radians."""
    return primitive1(x)


def tan(x: TTerm) -> TTerm:
    """Return the tangent of x radians."""
    return primitive1(x)


# Hyperbolic functions

def acosh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic cosine of x."""
    return primitive1(x)


def asinh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic sine of x."""
    return primitive1(x)


def atanh(x: TTerm) -> TTerm:
    """Return the inverse hyperbolic tangent of x."""
    return primitive1(x)


def cosh(x: TTerm) -> TTerm:
    """Return the hyperbolic cosine of x."""
    return primitive1(x)


def sinh(x: TTerm) -> TTerm:
    """Return the hyperbolic sine of x."""
    return primitive1(x)


def tanh(x: TTerm) -> TTerm:
    """Return the hyperbolic tangent of x."""
    return primitive1(x)


# Power and logarithmic functions

def exp(x: TTerm) -> TTerm:
    """Return e raised to the power x."""
    return primitive1(x)


def log(x: TTerm) -> TTerm:
    """Return the natural logarithm of x."""
    return primitive1(x)


def log_base(base: TTerm, x: TTerm) -> TTerm:
    """Return the logarithm of x to the given base."""
    return primitive2(base, x)


def pow_(x: TTerm, y: TTerm) -> TTerm:
    """Return x raised to the power y."""
    return primitive2(x, y)


def sqrt(x: TTerm) -> TTerm:
    """Return the square root of x."""
    return primitive1(x)


# Rounding functions

def ceiling(x: TTerm) -> TTerm:
    """Return the ceiling of x as an integer."""
    return primitive1(x)


def floor(x: TTerm) -> TTerm:
    """Return the floor of x as an integer."""
    return primitive1(x)


def round_(x: TTerm) -> TTerm:
    """Return x rounded to the nearest integer."""
    return primitive1(x)


def truncate(x: TTerm) -> TTerm:
    """Return x truncated to an integer."""
    return primitive1(x)
