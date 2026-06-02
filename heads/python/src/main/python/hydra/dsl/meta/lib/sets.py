"""Phantom-typed term DSL for the hydra.lib.sets library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2


def delete(x: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Delete an element from a set."""
    return primitive2(x, s)


def difference(s1: TypedTerm, s2: TypedTerm) -> TypedTerm:
    """Compute the difference of two sets."""
    return primitive2(s1, s2)


def empty() -> TypedTerm:
    """Create an empty set."""
    return primitive()


def from_list(xs: TypedTerm) -> TypedTerm:
    """Create a set from a list."""
    return primitive1(xs)


def insert(x: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Insert an element into a set."""
    return primitive2(x, s)


def intersection(s1: TypedTerm, s2: TypedTerm) -> TypedTerm:
    """Compute the intersection of two sets."""
    return primitive2(s1, s2)


def map(f: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Map a function over a set."""
    return primitive2(f, s)


def member(x: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Check if an element is in a set."""
    return primitive2(x, s)


def null(s: TypedTerm) -> TypedTerm:
    """Check if a set is empty."""
    return primitive1(s)


def singleton(x: TypedTerm) -> TypedTerm:
    """Create a singleton set."""
    return primitive1(x)


def size(s: TypedTerm) -> TypedTerm:
    """Get the size of a set."""
    return primitive1(s)


def to_list(s: TypedTerm) -> TypedTerm:
    """Convert a set to a sorted list."""
    return primitive1(s)


def union(s1: TypedTerm, s2: TypedTerm) -> TypedTerm:
    """Compute the union of two sets."""
    return primitive2(s1, s2)


def unions(sets: TypedTerm) -> TypedTerm:
    """Compute the union of multiple sets."""
    return primitive1(sets)
