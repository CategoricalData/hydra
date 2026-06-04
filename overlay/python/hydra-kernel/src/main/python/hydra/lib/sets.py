"""Python implementations of hydra.lib.sets primitives.

Inputs accept any ``AbstractSet``; outputs are ``PersistentSet`` instances
(returned as ``AbstractSet[A]``). ``PersistentSet`` is structurally shared
through its underlying ``PersistentMap``, so chained primitives get O(log n)
updates instead of the O(n) full-rebuild that ``frozenset`` imposed.
"""

from __future__ import annotations
from collections.abc import Callable, Sequence
from collections.abc import Set as AbstractSet
from typing import Any, TypeVar

from hydra.python.util import ConsList, PersistentSet

A = TypeVar('A')
B = TypeVar('B')


def _to_pset(s: AbstractSet[A]) -> PersistentSet[A]:
    if isinstance(s, PersistentSet):
        return s
    return PersistentSet.from_iterable(s)


def delete(x: A, s: AbstractSet[A]) -> AbstractSet[A]:
    """Delete an element from a set."""
    return _to_pset(s).delete(x)


def difference(s1: AbstractSet[A], s2: AbstractSet[A]) -> AbstractSet[A]:
    """Compute the difference of two sets."""
    return _to_pset(s1).difference(_to_pset(s2))


def empty() -> AbstractSet[Any]:
    """Create an empty set."""
    return PersistentSet.empty()


def from_list(xs: Sequence[A]) -> AbstractSet[A]:
    """Create a set from a list."""
    return PersistentSet.from_iterable(xs)


def insert(x: A, s: AbstractSet[A]) -> AbstractSet[A]:
    """Insert an element into a set. O(log n)."""
    return _to_pset(s).insert(x)


def intersection(s1: AbstractSet[A], s2: AbstractSet[A]) -> AbstractSet[A]:
    """Compute the intersection of two sets."""
    return _to_pset(s1).intersection(_to_pset(s2))


def map(f: Callable[[A], B], s: AbstractSet[A]) -> AbstractSet[B]:
    """Map a function over a set."""
    return PersistentSet.from_iterable(f(x) for x in s)


def member(x: A, s: AbstractSet[A]) -> bool:
    """Check if an element is in a set."""
    return x in s


def null(s: AbstractSet[Any]) -> bool:
    """Check if a set is empty."""
    return len(s) == 0


def singleton(x: A) -> AbstractSet[A]:
    """Create a singleton set."""
    return PersistentSet.singleton(x)


def size(s: AbstractSet[Any]) -> int:
    """Get the size of a set."""
    return len(s)


def to_list(s: AbstractSet[A]) -> Sequence[A]:
    """Convert a set to a list, in element order."""
    return ConsList.from_iterable(_to_pset(s).to_list())


def union(s1: AbstractSet[A], s2: AbstractSet[A]) -> AbstractSet[A]:
    """Compute the union of two sets."""
    return _to_pset(s1).union(_to_pset(s2))


def unions(sets: Sequence[AbstractSet[A]]) -> AbstractSet[A]:
    """Compute the union of multiple sets."""
    return PersistentSet.unions(_to_pset(s) for s in sets)
