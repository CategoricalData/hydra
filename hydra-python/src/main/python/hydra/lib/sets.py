"""Python implementations of hydra.lib.sets primitives."""

from collections.abc import Callable, Sequence
from typing import Any

from hydra.dsl.python import frozenlist


def delete[A](x: A, s: frozenset[A]) -> frozenset[A]:
    """Delete an element from a set."""
    return s - {x}


def difference[A](s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the difference of two sets."""
    return s1 - s2


def empty() -> frozenset[Any]:
    """Create an empty set."""
    return frozenset()


def from_list[A](xs: Sequence[A]) -> frozenset[A]:
    """Create a set from a list."""
    return frozenset(xs)


def insert[A](x: A, s: frozenset[A]) -> frozenset[A]:
    """Insert an element into a set."""
    return s | {x}


def intersection[A](s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the intersection of two sets."""
    return s1 & s2


def map[A, B](f: Callable[[A], B], s: frozenset[A]) -> frozenset[B]:
    """Map a function over a set."""
    return frozenset(f(x) for x in s)


def member[A](x: A, s: frozenset[A]) -> bool:
    """Check if an element is in a set."""
    return x in s


def null(s: frozenset[Any]) -> bool:
    """Check if a set is empty."""
    return len(s) == 0


def singleton[A](x: A) -> frozenset[A]:
    """Create a singleton set."""
    return frozenset({x})


def size(s: frozenset[Any]) -> int:
    """Get the size of a set."""
    return len(s)


# TODO: ensure that Hydra's native comparison primitives are used for sorting
def to_list[A](s: frozenset[A]) -> frozenlist[A]:
    """Convert a set to a sorted list."""
    return tuple(sorted(s))  # type: ignore[type-var]


def union[A](s1: frozenset[A], s2: frozenset[A]) -> frozenset[A]:
    """Compute the union of two sets."""
    return s1 | s2


def unions[A](sets: Sequence[frozenset[A]]) -> frozenset[A]:
    """Compute the union of multiple sets."""
    result = frozenset[A]()
    for s in sets:
        result = result | s
    return result

