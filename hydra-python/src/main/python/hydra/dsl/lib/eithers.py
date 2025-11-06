"""Phantom-typed term DSL for the hydra.lib.eithers library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def either(f: TTerm, g: TTerm, e: TTerm) -> TTerm:
    """Eliminate an Either value by applying one of two functions."""
    return primitive3(f, g, e)


def is_left(e: TTerm) -> TTerm:
    """Check if an Either is a Left value."""
    return primitive1(e)


def is_right(e: TTerm) -> TTerm:
    """Check if an Either is a Right value."""
    return primitive1(e)


def map_left(f: TTerm, e: TTerm) -> TTerm:
    """Map a function over the Left side of an Either."""
    return primitive2(f, e)


def map_right(f: TTerm, e: TTerm) -> TTerm:
    """Map a function over the Right side of an Either."""
    return primitive2(f, e)


def from_left(default: TTerm, e: TTerm) -> TTerm:
    """Extract the Left value, or return a default."""
    return primitive2(default, e)


def from_right(default: TTerm, e: TTerm) -> TTerm:
    """Extract the Right value, or return a default."""
    return primitive2(default, e)
