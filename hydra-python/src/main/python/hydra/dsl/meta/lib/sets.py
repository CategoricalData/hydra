"""Phantom-typed term DSL for the hydra.lib.sets library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2


def delete(x: TTerm, s: TTerm) -> TTerm:
    """Delete an element from a set."""
    return primitive2(x, s)


def difference(s1: TTerm, s2: TTerm) -> TTerm:
    """Compute the difference of two sets."""
    return primitive2(s1, s2)


def empty() -> TTerm:
    """Create an empty set."""
    return primitive()


def from_list(xs: TTerm) -> TTerm:
    """Create a set from a list."""
    return primitive1(xs)


def insert(x: TTerm, s: TTerm) -> TTerm:
    """Insert an element into a set."""
    return primitive2(x, s)


def intersection(s1: TTerm, s2: TTerm) -> TTerm:
    """Compute the intersection of two sets."""
    return primitive2(s1, s2)


def map(f: TTerm, s: TTerm) -> TTerm:
    """Map a function over a set."""
    return primitive2(f, s)


def member(x: TTerm, s: TTerm) -> TTerm:
    """Check if an element is in a set."""
    return primitive2(x, s)


def null(s: TTerm) -> TTerm:
    """Check if a set is empty."""
    return primitive1(s)


def singleton(x: TTerm) -> TTerm:
    """Create a singleton set."""
    return primitive1(x)


def size(s: TTerm) -> TTerm:
    """Get the size of a set."""
    return primitive1(s)


def to_list(s: TTerm) -> TTerm:
    """Convert a set to a sorted list."""
    return primitive1(s)


def union(s1: TTerm, s2: TTerm) -> TTerm:
    """Compute the union of two sets."""
    return primitive2(s1, s2)


def unions(sets: TTerm) -> TTerm:
    """Compute the union of multiple sets."""
    return primitive1(sets)
