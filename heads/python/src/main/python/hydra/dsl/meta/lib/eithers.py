"""Phantom-typed term DSL for the hydra.lib.eithers library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def bimap(f: TypedTerm, g: TypedTerm, e: TypedTerm) -> TypedTerm:
    """Map over both sides of an Either value."""
    return primitive3(f, g, e)


def bind(e: TypedTerm, f: TypedTerm) -> TypedTerm:
    """Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged."""
    return primitive2(e, f)


def either(f: TypedTerm, g: TypedTerm, e: TypedTerm) -> TypedTerm:
    """Eliminate an Either value by applying one of two functions."""
    return primitive3(f, g, e)


def foldl(f: TypedTerm, init: TypedTerm, xs: TypedTerm) -> TypedTerm:
    """Left-fold over a list with an Either-returning function, short-circuiting on Left."""
    return primitive3(f, init, xs)


def from_left(default: TypedTerm, e: TypedTerm) -> TypedTerm:
    """Extract the Left value, or return a default."""
    return primitive2(default, e)


def from_right(default: TypedTerm, e: TypedTerm) -> TypedTerm:
    """Extract the Right value, or return a default."""
    return primitive2(default, e)


def is_left(e: TypedTerm) -> TypedTerm:
    """Check if an Either is a Left value."""
    return primitive1(e)


def is_right(e: TypedTerm) -> TypedTerm:
    """Check if an Either is a Right value."""
    return primitive1(e)


def lefts(eithers: TypedTerm) -> TypedTerm:
    """Extract all Left values from a list of Eithers."""
    return primitive1(eithers)


def map_(f: TypedTerm, e: TypedTerm) -> TypedTerm:
    """Map a function over the Right side of an Either (standard functor map)."""
    return primitive2(f, e)


def map_list(f: TypedTerm, xs: TypedTerm) -> TypedTerm:
    """Map a function returning Either over a list, collecting results or short-circuiting on Left."""
    return primitive2(f, xs)


def map_maybe(f: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Map a function returning Either over a Maybe, or return Right Nothing if Nothing."""
    return primitive2(f, x)


def map_set(f: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Map a function returning Either over a Set, collecting results or short-circuiting on Left."""
    return primitive2(f, s)


def partition_eithers(eithers: TypedTerm) -> TypedTerm:
    """Partition a list of Eithers into lefts and rights."""
    return primitive1(eithers)


def rights(eithers: TypedTerm) -> TypedTerm:
    """Extract all Right values from a list of Eithers."""
    return primitive1(eithers)
