"""Python implementations of hydra.lib.maps primitives.

Inputs accept any ``Mapping``; outputs are ``PersistentMap`` instances
(returned as ``Mapping[K, V]``). ``PersistentMap`` is a structurally-shared
red-black tree, so chained primitives — typical of inference-style workloads —
get O(log n) updates instead of the O(n) full-dict-copy that ``FrozenDict``
imposed.
"""

from __future__ import annotations
from collections.abc import Callable, Mapping, Sequence
from typing import Any, TypeVar

from hydra.dsl.python import Maybe, Just, Nothing, NOTHING
from hydra.python.util import ConsList, PersistentMap

K = TypeVar('K')
K1 = TypeVar('K1')
K2 = TypeVar('K2')
V = TypeVar('V')
V1 = TypeVar('V1')
V2 = TypeVar('V2')


def _to_pmap(mapping: Mapping[K, V]) -> PersistentMap[K, V]:
    if isinstance(mapping, PersistentMap):
        return mapping
    return PersistentMap.from_mapping(mapping)


def alter(
    f: Callable[[Maybe[V]], Maybe[V]], key: K, mapping: Mapping[K, V]
) -> Mapping[K, V]:
    """Alter a value at a key using a function."""
    current_value = lookup(key, mapping)
    new_value = f(current_value)
    if isinstance(new_value, Just):
        return insert(key, new_value.value, mapping)
    return delete(key, mapping)


def bimap(
    f: Callable[[K1], K2], g: Callable[[V1], V2], mapping: Mapping[K1, V1]
) -> Mapping[K2, V2]:
    """Map a function over the keys and values of a map."""
    return PersistentMap.from_pairs((f(k), g(v)) for k, v in mapping.items())


def delete(key: K, mapping: Mapping[K, V]) -> Mapping[K, V]:
    """Remove a key from a map."""
    return _to_pmap(mapping).delete(key)


def elems(mapping: Mapping[Any, V]) -> Sequence[V]:
    """Get the values of a map, in key order."""
    return ConsList.from_iterable(_to_pmap(mapping).values_list())


def empty() -> Mapping[Any, Any]:
    """Create an empty map."""
    return PersistentMap.empty()


def filter(predicate: Callable[[V], bool], mapping: Mapping[K, V]) -> Mapping[K, V]:
    """Filter a map based on values."""
    return _to_pmap(mapping).filter(predicate)


def filter_with_key(
    predicate: Callable[[K, V], bool], mapping: Mapping[K, V]
) -> Mapping[K, V]:
    """Filter a map based on key-value pairs."""
    return _to_pmap(mapping).filter_with_key(predicate)


def find_with_default(default: V | Callable[[], V], key: K, mapping: Mapping[K, V]) -> V:
    """Lookup a value with a default. The default is lazy (#391)."""
    if isinstance(mapping, PersistentMap):
        if mapping.contains_key(key):
            return mapping[key]
    elif key in mapping:
        return mapping[key]
    return default() if callable(default) else default  # type: ignore[return-value]


def from_list(pairs: Sequence[tuple[K, V]]) -> Mapping[K, V]:
    """Create a map from a list of key-value pairs."""
    return PersistentMap.from_pairs(pairs)


def insert(key: K, value: V, mapping: Mapping[K, V]) -> Mapping[K, V]:
    """Insert a key-value pair into a map. O(log n)."""
    return _to_pmap(mapping).insert(key, value)


def keys(mapping: Mapping[K, Any]) -> Sequence[K]:
    """Get the keys of a map, in sorted order."""
    return ConsList.from_iterable(_to_pmap(mapping).keys_list())


def lookup(key: K, mapping: Mapping[K, V]) -> Maybe[V]:
    """Lookup a value in a map."""
    if isinstance(mapping, PersistentMap):
        if mapping.contains_key(key):
            return Just(mapping[key])
        return NOTHING
    if key in mapping:
        return Just(mapping[key])
    return NOTHING


def map(f: Callable[[V1], V2], mapping: Mapping[K, V1]) -> Mapping[K, V2]:
    """Map a function over the values of a map."""
    return _to_pmap(mapping).map_values(f)


def map_keys(
    f: Callable[[K1], K2], mapping: Mapping[K1, V]
) -> Mapping[K2, V]:
    """Map a function over the keys of a map."""
    return _to_pmap(mapping).map_keys(f)


def member(key: K, mapping: Mapping[K, Any]) -> bool:
    """Check if a key is present in a map."""
    return key in mapping


def null(mapping: Mapping[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def singleton(key: K, value: V) -> Mapping[K, V]:
    """Create a map with a single key-value pair."""
    return PersistentMap.singleton(key, value)


def size(mapping: Mapping[Any, Any]) -> int:
    """Get the size of a map."""
    return len(mapping)


def to_list(mapping: Mapping[K, V]) -> Sequence[tuple[K, V]]:
    """Convert a map to a list of key-value pairs, in sorted-key order."""
    return ConsList.from_iterable(_to_pmap(mapping).to_list())


def union(map1: Mapping[K, V], map2: Mapping[K, V]) -> Mapping[K, V]:
    """Union two maps, with the first taking precedence. O(m * log(m + n))."""
    return _to_pmap(map1).union(_to_pmap(map2))
