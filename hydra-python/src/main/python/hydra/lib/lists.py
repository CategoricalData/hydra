"""Python implementations of hydra.lib.lists primitives."""

from collections.abc import Callable
from functools import reduce
from typing import Any


def apply[A, B](f: Callable[[A], B], x: A) -> B:
    """Apply a function to an argument."""
    return f(x)


def at[A](i: int, values: list[A]) -> A:
    """Get the element at specified index of a list."""
    return values[i]


def bind[A, B](values: list[A], f: Callable[[A], list[B]]) -> list[B]:
    """Apply a function that returns lists to each element and flatten results."""
    return [s for v in values for s in f(v)]


def concat[A](values: list[list[A]]) -> list[A]:
    """Concatenate a list of lists."""
    return [v for vs in values for v in vs]


def concat2[A](values1: list[A], values2: list[A]) -> list[A]:
    """Concatenate two lists."""
    return values1 + values2


def cons[A](value: A, values: list[A]) -> list[A]:
    """Prepend a value to a list."""
    return [value, *values]


def filter[A](f: Callable[[A], bool], values: list[A]) -> list[A]:
    """Filter a list based on a predicate."""
    return [v for v in values if f(v)]


def foldl[A, B](f: Callable[[B, A], B], initial: B, values: list[A]) -> B:
    """Fold a list from the left."""
    return reduce(f, values, initial)


def head[A](values: list[A]) -> A:
    """Get the first element of a list."""
    return values[0]


def intercalate[A](separator: list[A], values: list[list[A]]) -> list[A]:
    """Intercalate a list of lists."""
    return [x for xs in values[:-1] for x in (xs + separator)] + values[-1]


def intersperse[A](separator: A, values: list[A]) -> list[A]:
    """Intersperse a value between elements of a list."""
    return [x for pair in zip(values, [separator] * len(values)) for x in pair][:-1]


def last[A](values: list[A]) -> A:
    """Get the last element of a list."""
    return values[-1]


def length(values: list[Any]) -> int:
    """Get the length of a list."""
    return len(values)


def map[A, B](f: Callable[[A], B], values: list[A]) -> list[B]:
    """Map a function over a list."""
    return [f(v) for v in values]


def nub[A](values: list[A]) -> list[A]:
    """Remove duplicate elements from a list."""
    seen: set[A] = set()
    return [x for x in values if not (x in seen or seen.add(x))]


def null(values: list[Any]) -> bool:
    """Check if a list is empty."""
    return len(values) == 0


def pure[A](value: A) -> list[A]:
    """Create a list with a single element."""
    return [value]


def reverse[A](values: list[A]) -> list[A]:
    """Reverse a list."""
    return values[::-1]


def safe_head[A](values: list[A]) -> A | None:
    """Get the first element of a list, returning None if the list is empty."""
    return values[0] if len(values) > 0 else None


def tail[A](values: list[A]) -> list[A]:
    """Get all elements of a list except the first."""
    return values[1:]
