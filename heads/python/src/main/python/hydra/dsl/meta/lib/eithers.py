"""Phantom-typed term DSL for the hydra.lib.eithers library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def either(f: TTerm, g: TTerm, e: TTerm) -> TTerm:
    """Eliminate an Either value by applying one of two functions."""
    return primitive3(f, g, e)


def from_left(default: TTerm, e: TTerm) -> TTerm:
    """Extract the Left value, or return a default."""
    return primitive2(default, e)


def from_right(default: TTerm, e: TTerm) -> TTerm:
    """Extract the Right value, or return a default."""
    return primitive2(default, e)


def is_left(e: TTerm) -> TTerm:
    """Check if an Either is a Left value."""
    return primitive1(e)


def is_right(e: TTerm) -> TTerm:
    """Check if an Either is a Right value."""
    return primitive1(e)


def lefts(eithers: TTerm) -> TTerm:
    """Extract all Left values from a list of Eithers."""
    return primitive1(eithers)


def partition_eithers(eithers: TTerm) -> TTerm:
    """Partition a list of Eithers into lefts and rights."""
    return primitive1(eithers)


def rights(eithers: TTerm) -> TTerm:
    """Extract all Right values from a list of Eithers."""
    return primitive1(eithers)
