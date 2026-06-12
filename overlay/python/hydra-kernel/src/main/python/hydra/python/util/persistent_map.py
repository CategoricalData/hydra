"""Persistent ordered map backed by a frozen ``dict`` (CPython C dict).

Mirrors ``hydra.util.PersistentMap`` from Hydra-Java. Iteration via
``keys()`` / ``values()`` / ``items()`` / ``__iter__`` is in **key-sorted
order** (Haskell semantics), achieved by sorting at iteration time using
``hydra.python.util._compare.compare``.

This implementation uses a native ``dict`` as the backing store. Mutating
methods (``insert`` / ``delete`` / ``union`` / ``map_values`` / etc.) build a
fresh ``dict`` and freeze it via ``_internal=True``. Native ``dict``
operations are C-speed; the cost is O(n) copy on every mutation rather than
O(log n) HAMT structure sharing.

Sorting is done with ``_compare.compare`` so that keys without a natural
``<`` (Hydra ``Term`` / ``Type`` / dataclasses) still get a deterministic
order.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Iterator, Mapping
from functools import cmp_to_key, total_ordering
from typing import Any, Generic, TypeVar

from hydra.python.util._compare import compare as _cmp

K = TypeVar("K")
K2 = TypeVar("K2")
V = TypeVar("V")
V2 = TypeVar("V2")
R = TypeVar("R")

_KEY_SORT_KEY = cmp_to_key(_cmp)


@total_ordering
class PersistentMap(Mapping[K, V], Generic[K, V]):
    """A persistent (immutable) map backed by a frozen native ``dict``.

    Construct via ``PersistentMap.empty()``, ``PersistentMap.singleton(k, v)``,
    ``PersistentMap.from_pairs(...)``, or ``PersistentMap.from_mapping(...)``.
    """

    __slots__ = ("_inner",)

    _EMPTY: PersistentMap[Any, Any]  # set after class body

    def __init__(self, _inner: dict[Any, Any] | None = None, *, _internal: bool = False) -> None:
        if not _internal:
            raise TypeError(
                "PersistentMap cannot be constructed directly; use PersistentMap.empty(), "
                "PersistentMap.singleton(k, v), PersistentMap.from_pairs(...), or "
                "PersistentMap.from_mapping(...)"
            )
        self._inner: dict[Any, Any] = _inner if _inner is not None else {}

    # --- Factories ---

    @staticmethod
    def empty() -> PersistentMap[Any, Any]:
        return PersistentMap._EMPTY

    @staticmethod
    def singleton(key: K, value: V) -> PersistentMap[K, V]:
        return PersistentMap({key: value}, _internal=True)

    @staticmethod
    def from_pairs(pairs: Iterable[tuple[K, V]]) -> PersistentMap[K, V]:
        d = dict(pairs)
        if not d:
            return PersistentMap._EMPTY
        return PersistentMap(d, _internal=True)

    @staticmethod
    def from_mapping(source: Mapping[K, V]) -> PersistentMap[K, V]:
        if isinstance(source, PersistentMap):
            return source
        d = dict(source)
        if not d:
            return PersistentMap._EMPTY
        return PersistentMap(d, _internal=True)

    @staticmethod
    def of_entries(*entries: tuple[K, V]) -> PersistentMap[K, V]:
        return PersistentMap.from_pairs(entries)

    # --- Core accessors ---

    def __len__(self) -> int:
        return len(self._inner)

    def is_empty(self) -> bool:
        return len(self._inner) == 0

    def lookup(self, key: K) -> Any | None:
        return self._inner.get(key)

    def contains_key(self, key: K) -> bool:
        return key in self._inner

    def __getitem__(self, key: K) -> V:
        v = self._inner.get(key, _MISSING)
        if v is _MISSING:
            raise KeyError(key)
        return v  # type: ignore[return-value]

    def __contains__(self, key: object) -> bool:
        try:
            return key in self._inner
        except TypeError:
            return False

    # --- Updates (O(n) copy) ---

    def insert(self, key: K, value: V) -> PersistentMap[K, V]:
        existing = self._inner.get(key, _MISSING)
        if existing is value or (existing is not _MISSING and existing == value):
            return self
        new = dict(self._inner)
        new[key] = value
        return PersistentMap(new, _internal=True)

    def delete(self, key: K) -> PersistentMap[K, V]:
        if key not in self._inner:
            return self
        new = dict(self._inner)
        del new[key]
        if not new:
            return PersistentMap._EMPTY
        return PersistentMap(new, _internal=True)

    def union(self, other: PersistentMap[K, V]) -> PersistentMap[K, V]:
        """Left-biased union: entries in ``self`` win on key collision.

        Implemented as ``{**other._inner, **self._inner}``: build other first,
        then overlay self → self values overwrite other on collision (left-biased).
        Pure C-speed, no Python-level loop.
        """
        if not self._inner:
            return other
        if not other._inner:
            return self
        return PersistentMap({**other._inner, **self._inner}, _internal=True)

    def alter(
        self,
        f: Callable[[Any], Any],
        key: K,
    ) -> PersistentMap[K, V]:
        current = self._inner.get(key)
        new = f(current)
        if new is None:
            return self.delete(key)
        return self.insert(key, new)

    def map_values(self, f: Callable[[V], V2]) -> PersistentMap[K, V2]:
        if not self._inner:
            return PersistentMap._EMPTY  # type: ignore[return-value]
        return PersistentMap({k: f(v) for k, v in self._inner.items()}, _internal=True)

    def filter(self, predicate: Callable[[V], bool]) -> PersistentMap[K, V]:
        return self.filter_with_key(lambda _, v: predicate(v))

    def filter_with_key(
        self, predicate: Callable[[K, V], bool]
    ) -> PersistentMap[K, V]:
        new = {k: v for k, v in self._inner.items() if predicate(k, v)}
        if len(new) == len(self._inner):
            return self
        if not new:
            return PersistentMap._EMPTY
        return PersistentMap(new, _internal=True)

    def map_keys(self, f: Callable[[K], K2]) -> PersistentMap[K2, V]:
        return PersistentMap.from_pairs((f(k), v) for k, v in self._inner.items())

    def bimap(
        self, fk: Callable[[K], K2], fv: Callable[[V], V2]
    ) -> PersistentMap[K2, V2]:
        return PersistentMap.from_pairs((fk(k), fv(v)) for k, v in self._inner.items())

    # --- Traversal helpers (sorted) ---

    def keys_list(self) -> list[K]:
        return sorted(self._inner.keys(), key=_KEY_SORT_KEY)

    def values_list(self) -> list[V]:
        return [self._inner[k] for k in self.keys_list()]

    def to_list(self) -> list[tuple[K, V]]:
        return [(k, self._inner[k]) for k in self.keys_list()]

    # --- Mapping protocol (sorted iteration: Haskell semantics) ---

    def __iter__(self) -> Iterator[K]:
        return iter(self.keys_list())

    def items(self) -> Any:
        return _ItemsView(self)

    def keys(self) -> Any:
        return _KeysView(self)

    def values(self) -> Any:
        return _ValuesView(self)

    def fold(self, f: Callable[[R, K, V], R], initial: R) -> R:
        acc = initial
        for k, v in self.items():
            acc = f(acc, k, v)
        return acc

    # --- Equality / hashing / repr ---

    def __eq__(self, other: object) -> bool:
        if isinstance(other, PersistentMap):
            return self._inner == other._inner
        if isinstance(other, Mapping):
            if len(other) != len(self._inner):
                return False
            for k, v in self._inner.items():
                if k not in other:
                    return False
                if other[k] != v:
                    return False
            return True
        return NotImplemented

    def __hash__(self) -> int:
        # dict isn't hashable; hash the sorted-items tuple.
        return hash(tuple(self.to_list()))

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, PersistentMap):
            return NotImplemented
        ai = iter(self.items())
        bi = iter(other.items())
        for (ak, av), (bk, bv) in zip(ai, bi):
            r = _cmp(ak, bk)
            if r != 0:
                return r < 0
            r = _cmp(av, bv)
            if r != 0:
                return r < 0
        return len(self._inner) < len(other._inner)

    def __repr__(self) -> str:
        body = ", ".join(f"{k!r}: {v!r}" for k, v in self.items())
        return f"PersistentMap({{{body}}})"


_MISSING: Any = object()


# --- Mapping views ---


class _ItemsView(Generic[K, V]):
    __slots__ = ("_map",)

    def __init__(self, m: PersistentMap[K, V]) -> None:
        self._map = m

    def __iter__(self) -> Iterator[tuple[K, V]]:
        return iter(self._map.to_list())

    def __len__(self) -> int:
        return len(self._map)

    def __contains__(self, item: object) -> bool:
        if not (isinstance(item, tuple) and len(item) == 2):
            return False
        k, v = item
        if not self._map.contains_key(k):  # type: ignore[arg-type]
            return False
        return self._map[k] == v  # type: ignore[index]


class _KeysView(Generic[K, V]):
    __slots__ = ("_map",)

    def __init__(self, m: PersistentMap[K, V]) -> None:
        self._map = m

    def __iter__(self) -> Iterator[K]:
        return iter(self._map.keys_list())

    def __len__(self) -> int:
        return len(self._map)

    def __contains__(self, key: object) -> bool:
        return key in self._map


class _ValuesView(Generic[K, V]):
    __slots__ = ("_map",)

    def __init__(self, m: PersistentMap[K, V]) -> None:
        self._map = m

    def __iter__(self) -> Iterator[V]:
        return iter(self._map.values_list())

    def __len__(self) -> int:
        return len(self._map)


PersistentMap._EMPTY = PersistentMap({}, _internal=True)
