"""Phantom-typed term DSL for the hydra.lib.math library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2


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
