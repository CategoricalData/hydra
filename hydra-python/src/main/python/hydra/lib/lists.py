"""Python implementations of hydra.lib.lists primitives."""

from collections.abc import Callable, Sequence
from functools import reduce
from typing import Any, TypeVar

from hydra.dsl.python import frozenlist

A = TypeVar('A')


def apply[A, B](fs: Sequence[Callable[[A], B]], values: Sequence[A]) -> frozenlist[B]:
    """Apply a list of functions to a list of values (applicative style)."""
    return tuple(f(v) for f in fs for v in values)


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


def elem[A](value: A, values: Sequence[A]) -> bool:
    """Check if an element is in a list."""
    return value in values


def filter[A](f: Callable[[A], bool], values: Sequence[A]) -> frozenlist[A]:
    """Filter a list based on a predicate."""
    return tuple(v for v in values if f(v))


def foldl[A, B](f: Callable[[B, A], B], initial: B, values: Sequence[A]) -> B:
    """Fold a list from the left."""
    return reduce(f, values, initial)


def group[A](values: Sequence[A]) -> frozenlist[frozenlist[A]]:
    """Group consecutive equal elements."""
    if not values:
        return ()

    result: list[tuple[A, ...]] = []
    current_group: list[A] = [values[0]]

    for i in range(1, len(values)):
        if values[i] == values[i-1]:
            current_group.append(values[i])
        else:
            result.append(tuple(current_group))
            current_group = [values[i]]

    result.append(tuple(current_group))
    return tuple(result)


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


def replicate[A](n: int, value: A) -> frozenlist[A]:
    """Create a list with n copies of a value."""
    return tuple(value for _ in range(n))


def reverse[A](values: Sequence[A]) -> frozenlist[A]:
    """Reverse a list."""
    return tuple(values[::-1])


def safe_head[A](values: Sequence[A]) -> A | None:
    """Get the first element of a list, returning None if the list is empty."""
    return values[0] if len(values) > 0 else None


def singleton[A](value: A) -> frozenlist[A]:
    """Create a single-element list."""
    return (value,)


# TODO: ensure that Hydra's native comparison primitives are used for sorting
def sort(values: Sequence[A]) -> frozenlist[A]:
    """Sort a list."""
    return tuple(sorted(values))  # type: ignore[type-var]

# TODO: ensure that Hydra's native comparison primitives are used for sorting
def sort_on[A, B](key: Callable[[A], B], values: Sequence[A]) -> frozenlist[A]:
    """Sort a list based on a key function."""
    return tuple(sorted(values, key=key))  # type: ignore[arg-type, type-var]


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


def transpose[A](values: Sequence[Sequence[A]]) -> frozenlist[frozenlist[A]]:
    """Transpose a list of lists."""
    if not values:
        return ()
    return tuple(tuple(row[i] for row in values if i < len(row)) for i in range(max(len(row) for row in values)))


def zip[A, B](values1: Sequence[A], values2: Sequence[B]) -> frozenlist[tuple[A, B]]:
    """Zip two lists into pairs."""
    import builtins
    return tuple(builtins.zip(values1, values2))


def zip_with[A, B, C](f: Callable[[A, B], C], values1: Sequence[A], values2: Sequence[B]) -> frozenlist[C]:
    """Zip two lists with a combining function."""
    import builtins
    return tuple(f(a, b) for a, b in builtins.zip(values1, values2))
