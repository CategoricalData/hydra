"""Phantom-typed term DSL for the hydra.lib.equality library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2


def compare(x: TTerm, y: TTerm) -> TTerm:
    """Compare two values and return a Comparison."""
    return primitive2(x, y)


def equal(a: TTerm, b: TTerm) -> TTerm:
    """Check if two values are equal."""
    return primitive2(a, b)


def gt(a: TTerm, b: TTerm) -> TTerm:
    """Check if first value is greater than second."""
    return primitive2(a, b)


def gte(a: TTerm, b: TTerm) -> TTerm:
    """Check if first value is greater than or equal to second."""
    return primitive2(a, b)


def identity(a: TTerm) -> TTerm:
    """Return the identity of a value."""
    return primitive1(a)


def lt(a: TTerm, b: TTerm) -> TTerm:
    """Check if first value is less than second."""
    return primitive2(a, b)


def lte(a: TTerm, b: TTerm) -> TTerm:
    """Check if first value is less than or equal to second."""
    return primitive2(a, b)


def max_(a: TTerm, b: TTerm) -> TTerm:
    """Return the maximum of two values."""
    return primitive2(a, b)


def min_(a: TTerm, b: TTerm) -> TTerm:
    """Return the minimum of two values."""
    return primitive2(a, b)
