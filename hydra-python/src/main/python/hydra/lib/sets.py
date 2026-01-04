"""Python implementations of hydra.lib.sets primitives."""

from __future__ import annotations
from collections.abc import Callable, Sequence
from typing import Any, TypeVar

from hydra.dsl.python import frozenlist

A = TypeVar('A')
B = TypeVar('B')


def delete(x: A, s: frozenset[A]) -> frozenset[A]:
    """Delete an element from a set."""
    return s - {x}


def difference(s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the difference of two sets."""
    return s1 - s2


def empty() -> frozenset[Any]:
    """Create an empty set."""
    return frozenset()


def from_list(xs: Sequence[A]) -> frozenset[A]:
    """Create a set from a list."""
    return frozenset(xs)


def insert(x: A, s: frozenset[A]) -> frozenset[A]:
    """Insert an element into a set."""
    return s | {x}


def intersection(s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the intersection of two sets."""
    return s1 & s2


def map(f: Callable[[A], B], s: frozenset[A]) -> frozenset[B]:
    """Map a function over a set."""
    return frozenset(f(x) for x in s)


def member(x: A, s: frozenset[A]) -> bool:
    """Check if an element is in a set."""
    return x in s


def null(s: frozenset[Any]) -> bool:
    """Check if a set is empty."""
    return len(s) == 0


def singleton(x: A) -> frozenset[A]:
    """Create a singleton set."""
    return frozenset({x})


def size(s: frozenset[Any]) -> int:
    """Get the size of a set."""
    return len(s)


# TODO: ensure that Hydra's native comparison primitives are used for sorting
def to_list(s: frozenset[A]) -> frozenlist[A]:
    """Convert a set to a list.

    Attempts to sort if elements are comparable, otherwise returns unsorted.
    """
    try:
        return tuple(sorted(s))  # type: ignore[type-var]
    except TypeError:
        # Elements don't support comparison (e.g., Term objects)
        return tuple(s)


def union(s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the union of two sets."""
    return s1 | s2


def unions(sets: Sequence[frozenset[A]]) -> frozenset[A]:
    """Compute the union of multiple sets."""
    result = frozenset[A]()
    for s in sets:
        result = result | s
    return result

