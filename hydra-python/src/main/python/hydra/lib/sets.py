"""Python implementations of hydra.lib.sets primitives."""

from collections.abc import Callable
from typing import Any


def contains[A](x: A, s: set[A]) -> bool:
    """Check if an element is in a set."""
    return x in s


def difference[A](s1: set[A], s2: set[A]) -> set[A]:
    """Compute the difference of two sets."""
    return s1 - s2


def empty[A]():
    """Create an empty set."""
    return set[A]()


def from_list[A](xs: list[A]) -> set[A]:
    """Create a set from a list."""
    return set(xs)


def insert[A](x: A, s: set[A]) -> set[A]:
    """Insert an element into a set."""
    return s | {x}


def intersection[A](s1: set[A], s2: set[A]) -> set[A]:
    """Compute the intersection of two sets."""
    return s1 & s2


def is_empty(s: set[Any]) -> bool:
    """Check if a set is empty."""
    return len(s) == 0


def map[A, B](f: Callable[[A], B], s: set[A]) -> set[B]:
    """Map a function over a set."""
    return set(f(x) for x in s)


def remove[A](x: A, s: set[A]) -> set[A]:
    """Remove an element from a set."""
    return s - {x}


def singleton[A](x: A) -> set[A]:
    """Create a singleton set."""
    return {x}


def size(s: set[Any]) -> int:
    """Get the size of a set."""
    return len(s)


def to_list[A](s: set[A]) -> list[A]:
    """Convert a set to a list."""
    return list(s)


def union[A](s1: set[A], s2: set[A]) -> set[A]:
    """Compute the union of two sets."""
    return s1 | s2
