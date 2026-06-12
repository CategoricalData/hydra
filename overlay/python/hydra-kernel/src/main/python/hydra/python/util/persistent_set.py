"""Persistent ordered set, backed by a frozen ``frozenset``.

Mirrors ``hydra.util.PersistentSet`` from Hydra-Java. Iteration is in
sorted-element order. Implements ``collections.abc.Set`` so a
``PersistentSet`` IS a Set.

This implementation uses a native ``frozenset`` as the backing store. Native
operations (``in``, ``|``, ``&``, ``-``) are C-speed; the cost is full O(n)
copies on update operations rather than O(log n) HAMT structure sharing.

Elements are ordered via ``hydra.python.util._compare.compare`` (natural
``<`` first, structural fallback for Hydra ``Term``/``Type`` and other
complex values).
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator
from collections.abc import Set as AbstractSet
from functools import cmp_to_key, total_ordering
from typing import Any, Generic, TypeVar

from hydra.python.util._compare import compare as _cmp

T = TypeVar("T")
U = TypeVar("U")

_KEY_SORT_KEY = cmp_to_key(_cmp)


@total_ordering
class PersistentSet(AbstractSet[T], Generic[T]):
    """A persistent (immutable) ordered set backed by ``frozenset``."""

    __slots__ = ("_inner",)

    _EMPTY: PersistentSet[Any]  # set after class body

    def __init__(self, _inner: frozenset[Any] | None = None, *, _internal: bool = False) -> None:
        if not _internal:
            raise TypeError(
                "PersistentSet cannot be constructed directly; use PersistentSet.empty(), "
                "PersistentSet.singleton(x), PersistentSet.of(...), or "
                "PersistentSet.from_iterable(...)"
            )
        self._inner: frozenset[Any] = _inner if _inner is not None else frozenset()

    # --- Factories ---

    @staticmethod
    def empty() -> PersistentSet[Any]:
        return PersistentSet._EMPTY

    @staticmethod
    def singleton(value: T) -> PersistentSet[T]:
        return PersistentSet(frozenset({value}), _internal=True)

    @staticmethod
    def of(*elements: T) -> PersistentSet[T]:
        return PersistentSet.from_iterable(elements)

    @staticmethod
    def from_iterable(values: Iterable[T]) -> PersistentSet[T]:
        if isinstance(values, PersistentSet):
            return values
        fs = frozenset(values)
        if not fs:
            return PersistentSet._EMPTY
        return PersistentSet(fs, _internal=True)

    # --- Core ---

    def __len__(self) -> int:
        return len(self._inner)

    def is_empty(self) -> bool:
        return not self._inner

    def member(self, element: T) -> bool:
        return element in self._inner

    def __contains__(self, element: object) -> bool:
        try:
            return element in self._inner
        except TypeError:
            return False

    def __iter__(self) -> Iterator[T]:
        return iter(sorted(self._inner, key=_KEY_SORT_KEY))

    # --- Updates ---

    def insert(self, element: T) -> PersistentSet[T]:
        if element in self._inner:
            return self
        return PersistentSet(self._inner | {element}, _internal=True)

    def delete(self, element: T) -> PersistentSet[T]:
        if element not in self._inner:
            return self
        new = self._inner - {element}
        if not new:
            return PersistentSet._EMPTY
        return PersistentSet(new, _internal=True)

    def union(self, other: PersistentSet[T]) -> PersistentSet[T]:
        if not self._inner:
            return other
        if not other._inner:
            return self
        return PersistentSet(self._inner | other._inner, _internal=True)

    def intersection(self, other: PersistentSet[T]) -> PersistentSet[T]:
        if not self._inner or not other._inner:
            return PersistentSet._EMPTY
        new = self._inner & other._inner
        if not new:
            return PersistentSet._EMPTY
        return PersistentSet(new, _internal=True)

    def difference(self, other: PersistentSet[T]) -> PersistentSet[T]:
        if not self._inner:
            return self
        if not other._inner:
            return self
        new = self._inner - other._inner
        if not new:
            return PersistentSet._EMPTY
        return PersistentSet(new, _internal=True)

    @staticmethod
    def unions(sets: Iterable[PersistentSet[T]]) -> PersistentSet[T]:
        # Collect all inner frozensets, single C-level union
        result: frozenset[Any] = frozenset()
        for s in sets:
            if isinstance(s, PersistentSet):
                result = result | s._inner
            else:
                result = result | frozenset(s)
        if not result:
            return PersistentSet._EMPTY
        return PersistentSet(result, _internal=True)

    def map(self, f: Callable[[T], U]) -> PersistentSet[U]:
        return PersistentSet.from_iterable(f(x) for x in self._inner)

    def filter(self, predicate: Callable[[T], bool]) -> PersistentSet[T]:
        kept = frozenset(x for x in self._inner if predicate(x))
        if len(kept) == len(self._inner):
            return self
        if not kept:
            return PersistentSet._EMPTY
        return PersistentSet(kept, _internal=True)

    # --- Conversions ---

    def to_list(self) -> list[T]:
        return sorted(self._inner, key=_KEY_SORT_KEY)

    # --- Equality / hash / repr ---

    def __eq__(self, other: object) -> bool:
        if isinstance(other, PersistentSet):
            return self._inner == other._inner
        if isinstance(other, AbstractSet):
            return len(self) == len(other) and all(x in other for x in self._inner)
        return NotImplemented

    def __hash__(self) -> int:
        return hash(self._inner)

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, PersistentSet):
            return NotImplemented
        ai = iter(self)
        bi = iter(other)
        for a, b in zip(ai, bi):
            r = _cmp(a, b)
            if r != 0:
                return r < 0
        return len(self._inner) < len(other._inner)

    def __repr__(self) -> str:
        body = ", ".join(repr(x) for x in self)
        return f"PersistentSet({{{body}}})"


PersistentSet._EMPTY = PersistentSet(frozenset(), _internal=True)
