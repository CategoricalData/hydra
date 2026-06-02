"""Phantom-typed term DSL for the hydra.lib.equality library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2


def compare(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Compare two values and return a Comparison."""
    return primitive2(x, y)


def equal(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Check if two values are equal."""
    return primitive2(a, b)


def gt(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Check if first value is greater than second."""
    return primitive2(a, b)


def gte(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Check if first value is greater than or equal to second."""
    return primitive2(a, b)


def identity(a: TypedTerm) -> TypedTerm:
    """Return the identity of a value."""
    return primitive1(a)


def lt(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Check if first value is less than second."""
    return primitive2(a, b)


def lte(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Check if first value is less than or equal to second."""
    return primitive2(a, b)


def max_(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Return the maximum of two values."""
    return primitive2(a, b)


def min_(a: TypedTerm, b: TypedTerm) -> TypedTerm:
    """Return the minimum of two values."""
    return primitive2(a, b)
