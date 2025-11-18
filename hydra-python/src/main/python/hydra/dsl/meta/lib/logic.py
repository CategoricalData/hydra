"""Phantom-typed term DSL for the hydra.lib.logic library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def and_(x: TTerm, y: TTerm) -> TTerm:
    """Compute the logical AND of two boolean values."""
    return primitive2(x, y)


def if_else(b: TTerm, x: TTerm, y: TTerm) -> TTerm:
    """Compute a conditional expression."""
    return primitive3(b, x, y)


def not_(x: TTerm) -> TTerm:
    """Compute the logical NOT of a boolean value."""
    return primitive1(x)


def or_(x: TTerm, y: TTerm) -> TTerm:
    """Compute the logical OR of two boolean values."""
    return primitive2(x, y)
