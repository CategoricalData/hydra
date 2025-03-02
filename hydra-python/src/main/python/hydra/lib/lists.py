"""Python implementations of hydra.lib.lists primitives."""

from collections.abc import Callable
from functools import reduce
from typing import Any

from hydra.dsl.python import frozenlist


def apply[A, B](f: Callable[[A], B], x: A) -> B:
    """Apply a function to an argument."""
    return f(x)


def at[A](i: int, values: frozenlist[A]) -> A:
    """Get the element at specified index of a list."""
    return values[i]


def bind[A, B](values: frozenlist[A], f: Callable[[A], frozenlist[B]]) -> frozenlist[B]:
    """Apply a function that returns lists to each element and flatten results."""
    return tuple(s for v in values for s in f(v))


def concat[A](values: frozenlist[frozenlist[A]]) -> frozenlist[A]:
    """Concatenate a list of lists."""
    return tuple(v for vs in values for v in vs)


def concat2[A](values1: list[A], values2: list[A]) -> list[A]:
    """Concatenate two lists."""
    return values1 + values2


def cons[A](value: A, values: list[A]) -> list[A]:
    """Prepend a value to a list."""
    return [value, *values]


def filter[A](f: Callable[[A], bool], values: frozenlist[A]) -> frozenlist[A]:
    """Filter a list based on a predicate."""
    return tuple(v for v in values if f(v))


def foldl[A, B](f: Callable[[B, A], B], initial: B, values: frozenlist[A]) -> B:
    """Fold a list from the left."""
    return reduce(f, values, initial)


def head[A](values: frozenlist[A]) -> A:
    """Get the first element of a list."""
    return values[0]


def intercalate[A](
    separator: frozenlist[A], values: frozenlist[frozenlist[A]]
) -> frozenlist[A]:
    """Intercalate a list of lists."""
    return tuple(x for xs in values[:-1] for x in (xs + separator)) + values[-1]


def intersperse[A](separator: A, values: frozenlist[A]) -> frozenlist[A]:
    """Intersperse a value between elements of a list."""
    return tuple(x for pair in zip(values, [separator] * len(values)) for x in pair)[
        :-1
    ]


def last[A](values: frozenlist[A]) -> A:
    """Get the last element of a list."""
    return values[-1]


def length(values: frozenlist[Any]) -> int:
    """Get the length of a list."""
    return len(values)


def map[A, B](f: Callable[[A], B], values: frozenlist[A]) -> frozenlist[B]:
    """Map a function over a list."""
    return tuple(f(v) for v in values)


def nub[A](values: frozenlist[A]) -> frozenlist[A]:
    """Remove duplicate elements from a list."""
    seen: set[A] = set()
    return tuple(x for x in values if not (x in seen or seen.add(x)))


def null(values: frozenlist[Any]) -> bool:
    """Check if a list is empty."""
    return len(values) == 0


def pure[A](value: A) -> frozenlist[A]:
    """Create a list with a single element."""
    return (value,)


def reverse[A](values: frozenlist[A]) -> frozenlist[A]:
    """Reverse a list."""
    return values[::-1]


def safe_head[A](values: frozenlist[A]) -> A | None:
    """Get the first element of a list, returning None if the list is empty."""
    return values[0] if len(values) > 0 else None


def tail[A](values: frozenlist[A]) -> frozenlist[A]:
    """Get all elements of a list except the first."""
    return values[1:]
