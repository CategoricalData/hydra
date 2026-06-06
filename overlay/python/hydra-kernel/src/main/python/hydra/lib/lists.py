"""Python implementations of hydra.lib.lists primitives.

Inputs accept any ``Sequence``; outputs are ``ConsList`` instances (returned as
``Sequence[A]``). ``ConsList`` is a structurally-shared persistent list, so
chained primitives — typical of inference-style workloads — avoid the O(n)
full-tuple-copy that ``tuple``-based implementations would impose.
"""

from __future__ import annotations
from collections.abc import Callable, Sequence
from functools import reduce
from typing import Any, TypeVar

from hydra.dsl.python import Optional, Given, None_
from hydra.python.util import ConsList

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def _to_cons(values: Sequence[A]) -> ConsList[A]:
    if isinstance(values, ConsList):
        return values
    return ConsList.from_iterable(values)


def apply(fs: Sequence[Callable[[A], B]], values: Sequence[A]) -> Sequence[B]:
    """Apply a list of functions to a list of values (applicative style)."""
    return ConsList.from_iterable(f(v) for f in fs for v in values)


def bind(values: Sequence[A], f: Callable[[A], Sequence[B]]) -> Sequence[B]:
    """Apply a function that returns lists to each element and flatten results."""
    return ConsList.from_iterable(s for v in values for s in f(v))


def concat(values: Sequence[Sequence[A]]) -> Sequence[A]:
    """Concatenate a list of lists."""
    return ConsList.from_iterable(v for vs in values for v in vs)


def concat2(values1: Sequence[A], values2: Sequence[A]) -> Sequence[A]:
    """Concatenate two lists."""
    a = _to_cons(values1)
    b = _to_cons(values2)
    return a.concat(b)


def cons(value: A, values: Sequence[A]) -> Sequence[A]:
    """Prepend a value to a list. O(1) when ``values`` is already a ``ConsList``."""
    return ConsList.cons(value, _to_cons(values))


def drop(n: int, values: Sequence[A]) -> Sequence[A]:
    """Drop the first n elements from a list.

    If n <= 0, returns the full list (Haskell semantics).
    """
    if n <= 0:
        return _to_cons(values)
    return _to_cons(values).drop(n)


def drop_while(predicate: Callable[[A], bool], values: Sequence[A]) -> Sequence[A]:
    """Drop elements from the beginning of a list while predicate is true."""
    cl = _to_cons(values)
    cur = cl
    while not cur.is_empty() and predicate(cur.head):
        cur = cur.tail
    return cur


def elem(value: A, values: Sequence[A]) -> bool:
    """Check if an element is in a list."""
    return value in values


def filter(f: Callable[[A], bool], values: Sequence[A]) -> Sequence[A]:
    """Filter a list based on a predicate."""
    return ConsList.from_iterable(v for v in values if f(v))


def find(predicate: Callable[[A], bool], values: Sequence[A]) -> Optional[A]:
    """Find the first element matching a predicate."""
    for v in values:
        if predicate(v):
            return Given(v)
    return None_()


def foldl(f: Callable[[B, A], B], initial: B, values: Sequence[A]) -> B:
    """Fold a list from the left."""
    return reduce(f, values, initial)


def foldr(f: Callable[[A, B], B], initial: B, values: Sequence[A]) -> B:
    """Fold a list from the right."""
    acc = initial
    for v in reversed(list(values)):
        acc = f(v, acc)
    return acc


def group(values: Sequence[A]) -> Sequence[Sequence[A]]:
    """Group consecutive equal elements."""
    if not values:
        return ConsList.empty()
    materialized = list(values)
    result: list[ConsList[A]] = []
    current_group: list[A] = [materialized[0]]
    for i in range(1, len(materialized)):
        if materialized[i] == materialized[i - 1]:
            current_group.append(materialized[i])
        else:
            result.append(ConsList.from_iterable(current_group))
            current_group = [materialized[i]]
    result.append(ConsList.from_iterable(current_group))
    return ConsList.from_iterable(result)


def intercalate(
    separator: Sequence[A], values: Sequence[Sequence[A]]
) -> Sequence[A]:
    """Intercalate a list of lists with a separator list between each.

    Returns empty list if values is empty (Haskell semantics).
    """
    materialized = list(values)
    if not materialized:
        return ConsList.empty()
    sep = list(separator)
    out: list[A] = []
    for i, vs in enumerate(materialized):
        if i > 0:
            out.extend(sep)
        out.extend(vs)
    return ConsList.from_iterable(out)


def intersperse(separator: A, values: Sequence[A]) -> Sequence[A]:
    """Intersperse a value between elements of a list."""
    materialized = list(values)
    if not materialized:
        return ConsList.empty()
    out: list[A] = [materialized[0]]
    for v in materialized[1:]:
        out.append(separator)
        out.append(v)
    return ConsList.from_iterable(out)


def length(values: Sequence[Any]) -> int:
    """Get the length of a list."""
    return len(values)


def map(f: Callable[[A], B], values: Sequence[A]) -> Sequence[B]:
    """Map a function over a list."""
    return ConsList.from_iterable(f(v) for v in values)


def maybe_at(i: int, values: Sequence[A]) -> Optional[A]:
    """Get the element at a specified index, returning Nothing if out of bounds."""
    return Given(values[i]) if 0 <= i < len(values) else None_()


def maybe_head(values: Sequence[A]) -> Optional[A]:
    """Get the first element of a list, returning Nothing if the list is empty."""
    return Given(values[0]) if len(values) > 0 else None_()


def maybe_init(values: Sequence[A]) -> Optional[Sequence[A]]:
    """Return all elements except the last, returning Nothing if empty."""
    if len(values) == 0:
        return None_()
    return Given(_to_cons(values).init())


def maybe_last(values: Sequence[A]) -> Optional[A]:
    """Get the last element of a list, returning Nothing if the list is empty."""
    return Given(values[-1]) if len(values) > 0 else None_()


def maybe_tail(values: Sequence[A]) -> Optional[Sequence[A]]:
    """Get all elements except the first, returning Nothing if empty."""
    if len(values) == 0:
        return None_()
    return Given(_to_cons(values).tail)


def nub(values: Sequence[A]) -> Sequence[A]:
    """Remove duplicate elements from a list."""
    seen: set[A] = set()
    out: list[A] = []
    for v in values:
        if v not in seen:
            seen.add(v)
            out.append(v)
    return ConsList.from_iterable(out)


def null(values: Sequence[Any]) -> bool:
    """Check if a list is empty."""
    return len(values) == 0


def partition(
    predicate: Callable[[A], bool], values: Sequence[A]
) -> tuple[Sequence[A], Sequence[A]]:
    """Partition a list into elements that satisfy a predicate and elements that do not."""
    yes: list[A] = []
    no: list[A] = []
    for v in values:
        if predicate(v):
            yes.append(v)
        else:
            no.append(v)
    return (ConsList.from_iterable(yes), ConsList.from_iterable(no))


def pure(value: A) -> Sequence[A]:
    """Create a list with a single element."""
    return ConsList.singleton(value)


def replicate(n: int, value: A) -> Sequence[A]:
    """Create a list with n copies of a value."""
    return ConsList.from_iterable(value for _ in range(n))


def reverse(values: Sequence[A]) -> Sequence[A]:
    """Reverse a list."""
    return _to_cons(values).reverse()


def singleton(value: A) -> Sequence[A]:
    """Create a single-element list."""
    return ConsList.singleton(value)


# TODO: ensure that Hydra's native comparison primitives are used for sorting
def sort(values: Sequence[A]) -> Sequence[A]:
    """Sort a list."""
    return ConsList.from_iterable(sorted(values))  # type: ignore[type-var]


# TODO: ensure that Hydra's native comparison primitives are used for sorting
def sort_on(key: Callable[[A], B], values: Sequence[A]) -> Sequence[A]:
    """Sort a list based on a key function."""
    return ConsList.from_iterable(sorted(values, key=key))  # type: ignore[arg-type, type-var]


def span(
    predicate: Callable[[A], bool], values: Sequence[A]
) -> tuple[Sequence[A], Sequence[A]]:
    """Split a list at the first element where predicate fails."""
    materialized = list(values)
    for i, value in enumerate(materialized):
        if not predicate(value):
            return (
                ConsList.from_iterable(materialized[:i]),
                ConsList.from_iterable(materialized[i:]),
            )
    return (ConsList.from_iterable(materialized), ConsList.empty())


def take(n: int, values: Sequence[A]) -> Sequence[A]:
    """Take the first n elements from a list.

    If n <= 0, returns an empty list (Haskell semantics).
    """
    if n <= 0:
        return ConsList.empty()
    return _to_cons(values).take(n)


def transpose(values: Sequence[Sequence[A]]) -> Sequence[Sequence[A]]:
    """Transpose a list of lists."""
    materialized = [list(row) for row in values]
    if not materialized:
        return ConsList.empty()
    width = max(len(row) for row in materialized)
    rows: list[ConsList[A]] = []
    for i in range(width):
        rows.append(
            ConsList.from_iterable(row[i] for row in materialized if i < len(row))
        )
    return ConsList.from_iterable(rows)


def uncons(values: Sequence[A]) -> Optional[tuple[A, Sequence[A]]]:
    """Split a list into its head and tail, returning Nothing if empty."""
    if len(values) == 0:
        return None_()
    cl = _to_cons(values)
    return Given((cl.head, cl.tail))


def zip(values1: Sequence[A], values2: Sequence[B]) -> Sequence[tuple[A, B]]:
    """Zip two lists into pairs."""
    import builtins
    return ConsList.from_iterable(builtins.zip(values1, values2))


def zip_with(
    f: Callable[[A, B], C], values1: Sequence[A], values2: Sequence[B]
) -> Sequence[C]:
    """Zip two lists with a combining function."""
    import builtins
    return ConsList.from_iterable(f(a, b) for a, b in builtins.zip(values1, values2))
