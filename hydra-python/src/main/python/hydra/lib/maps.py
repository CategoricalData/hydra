"""Python implementations of hydra.lib.maps primitives."""

from __future__ import annotations
from collections.abc import Callable, Mapping, Sequence
from typing import Any, TypeVar

from hydra.dsl.python import FrozenDict, frozenlist, Maybe, Just, Nothing, NOTHING

K = TypeVar('K')
K1 = TypeVar('K1')
K2 = TypeVar('K2')
V = TypeVar('V')
V1 = TypeVar('V1')
V2 = TypeVar('V2')


def alter(
    f: Callable[[Maybe[V]], Maybe[V]], key: K, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Alter a value at a key using a function."""
    current_value = lookup(key, mapping)
    new_value = f(current_value)
    match new_value:
        case Nothing():
            return delete(key, mapping)
        case Just(v):
            return insert(key, v, mapping)


def bimap(
    f: Callable[[K1], K2], g: Callable[[V1], V2], mapping: Mapping[K1, V1]) -> FrozenDict[K2, V2]:
    """Map a function over the keys and values of a map."""
    return FrozenDict({f(k): g(v) for k, v in mapping.items()})


def delete(key: K, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Remove a key from a map."""
    return FrozenDict({k: v for k, v in mapping.items() if k != key})


def elems(mapping: Mapping[Any, V]) -> frozenlist[V]:
    """Get the values of a map, in sorted key order."""
    return tuple(v for _, v in sorted(mapping.items()))


def empty() -> FrozenDict[Any, Any]:
    """Create an empty map."""
    return FrozenDict()


def filter(predicate: Callable[[V], bool], mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Filter a map based on values."""
    return FrozenDict({k: v for k, v in mapping.items() if predicate(v)})


def filter_with_key(
    predicate: Callable[[K, V], bool], mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Filter a map based on key-value pairs."""
    return FrozenDict({k: v for k, v in mapping.items() if predicate(k, v)})


def find_with_default(default: V, key: K, mapping: Mapping[K, V]) -> V:
    """Lookup a value with a default."""
    return mapping.get(key, default)


def from_list(pairs: Sequence[tuple[K, V]]) -> FrozenDict[K, V]:
    """Create a map from a list of key-value pairs."""
    return FrozenDict(dict(pairs))


def insert(key: K, value: V, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Insert a key-value pair into a map."""
    return FrozenDict({**mapping, key: value})


def keys(mapping: Mapping[K, Any]) -> frozenlist[K]:
    """Get the keys of a map, in sorted order."""
    return tuple(sorted(mapping.keys()))


def lookup(key: K, mapping: Mapping[K, V]) -> Maybe[V]:
    """Lookup a value in a map."""
    if key in mapping:
        return Just(mapping[key])
    else:
        return NOTHING


def map(f: Callable[[V1], V2], mapping: Mapping[K, V1]) -> FrozenDict[K, V2]:
    """Map a function over a map."""
    return FrozenDict[K, V2]({k: f(v) for k, v in mapping.items()})


def map_keys(
    f: Callable[[K1], K2], mapping: Mapping[K1, V]) -> FrozenDict[K2, V]:
    """Map a function over the keys of a map."""
    return FrozenDict({f(k): v for k, v in mapping.items()})


def member(key: K, mapping: Mapping[K, Any]) -> bool:
    """Check if a key is present in a map."""
    return key in mapping


def null(mapping: Mapping[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def singleton(key: K, value: V) -> FrozenDict[K, V]:
    """Create a map with a single key-value pair."""
    return FrozenDict({key: value})


def size(mapping: Mapping[Any, Any]) -> int:
    """Get the size of a map."""
    return len(mapping)


def to_list(mapping: Mapping[K, V]) -> frozenlist[tuple[K, V]]:
    """Convert a map to a list of key-value pairs, sorted by key."""
    return tuple(sorted(mapping.items()))


def union(map1: Mapping[K, V], map2: Mapping[K, V]) -> FrozenDict[K, V]:
    """Union two maps, with the first taking precedence."""
    return FrozenDict({**map2, **map1})
