"""Python implementations of hydra.lib.lists primitives."""

from collections.abc import Callable, Sequence
from functools import reduce
from typing import Any, TypeVar

from hydra.dsl.python import frozenlist

A = TypeVar('A')


def apply[A, B](f: Callable[[A], B], x: A) -> B:
    """Apply a function to an argument."""
    return f(x)


def at[A](i: int, values: Sequence[A]) -> A:
    """Get the element at specified index of a list."""
    return values[i]


def bind[A, B](values: Sequence[A], f: Callable[[A], Sequence[B]]) -> frozenlist[B]:
    """Apply a function that returns lists to each element and flatten results."""
    return tuple(s for v in values for s in f(v))


def concat[A](values: Sequence[Sequence[A]]) -> frozenlist[A]:
    """Concatenate a list of lists."""
    return tuple(v for vs in values for v in vs)


def concat2[A](values1: Sequence[A], values2: Sequence[A]) -> frozenlist[A]:
    """Concatenate two lists."""
    return tuple(values1) + tuple(values2)


def cons[A](value: A, values: Sequence[A]) -> frozenlist[A]:
    """Prepend a value to a list."""
    return (value, *values)


def drop(n: int, values: Sequence[A]) -> frozenlist[A]:
    """Drop the first n elements from a list."""
    return tuple(values[n:])


def drop_while(predicate: Callable[[A], bool], values: Sequence[A]) -> frozenlist[A]:
    """Drop elements from the beginning of a list while predicate is true."""
    for i, value in enumerate(values):
        if not predicate(value):
            return tuple(values[i:])
    return ()


def filter[A](f: Callable[[A], bool], values: Sequence[A]) -> frozenlist[A]:
    """Filter a list based on a predicate."""
    return tuple(v for v in values if f(v))


def foldl[A, B](f: Callable[[B, A], B], initial: B, values: Sequence[A]) -> B:
    """Fold a list from the left."""
    return reduce(f, values, initial)


def init(values: Sequence[A]) -> frozenlist[A]:
    """Return all elements except the last one."""
    return tuple(values[:-1])


def head[A](values: Sequence[A]) -> A:
    """Get the first element of a list."""
    return values[0]


def intercalate[A](
    separator: Sequence[A], values: Sequence[Sequence[A]]
) -> frozenlist[A]:
    """Intercalate a list of lists."""
    return tuple(x for xs in values[:-1] for x in concat2(xs, separator)) + tuple(
        values[-1]
    )


def intersperse[A](separator: A, values: Sequence[A]) -> frozenlist[A]:
    """Intersperse a value between elements of a list."""
    return tuple(x for pair in zip(values, [separator] * len(values)) for x in pair)[
        :-1
    ]


def last[A](values: Sequence[A]) -> A:
    """Get the last element of a list."""
    return values[-1]


def length(values: Sequence[Any]) -> int:
    """Get the length of a list."""
    return len(values)


def map[A, B](f: Callable[[A], B], values: Sequence[A]) -> frozenlist[B]:
    """Map a function over a list."""
    return tuple(f(v) for v in values)


def nub[A](values: Sequence[A]) -> frozenlist[A]:
    """Remove duplicate elements from a list."""
    seen: set[A] = set()
    return tuple(x for x in values if not (x in seen or seen.add(x)))


def null(values: Sequence[Any]) -> bool:
    """Check if a list is empty."""
    return len(values) == 0


def pure[A](value: A) -> frozenlist[A]:
    """Create a list with a single element."""
    return (value,)


def reverse[A](values: Sequence[A]) -> frozenlist[A]:
    """Reverse a list."""
    return tuple(values[::-1])


def safe_head[A](values: Sequence[A]) -> A | None:
    """Get the first element of a list, returning None if the list is empty."""
    return values[0] if len(values) > 0 else None


def span(predicate: Callable[[A], bool], values: Sequence[A]) -> tuple[frozenlist[A], frozenlist[A]]:
    """Split a list at the first element where predicate fails."""
    for i, value in enumerate(values):
        if not predicate(value):
            return (tuple(values[:i]), tuple(values[i:]))
    return (tuple(values), ())


def tail[A](values: Sequence[A]) -> frozenlist[A]:
    """Get all elements of a list except the first."""
    return tuple(values[1:])


def take(n: int, values: Sequence[A]) -> frozenlist[A]:
    """Take the first n elements from a list."""
    return tuple(values[:n])
