"""Python implementations of hydra.lib.maps primitives."""

from collections.abc import Callable, Mapping, Sequence
from typing import Any, Optional, TypeVar

from hydra.dsl.python import FrozenDict, frozenlist

K = TypeVar('K')
V = TypeVar('V')
K1 = TypeVar('K1')
K2 = TypeVar('K2')
V1 = TypeVar('V1')
V2 = TypeVar('V2')


def alter[K, V](
    f: Callable[[Optional[V]], Optional[V]], key: K, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Alter a value at a key using a function."""
    current_value = mapping.get(key)
    new_value = f(current_value)
    if new_value is None:
        return remove(key, mapping)
    else:
        return insert(key, new_value, mapping)


def bimap[K1, K2, V1, V2](
    f: Callable[[K1], K2], g: Callable[[V1], V2], mapping: Mapping[K1, V1]) -> FrozenDict[K2, V2]:
    """Map a function over the keys and values of a map."""
    return FrozenDict({f(k): g(v) for k, v in mapping.items()})


def elems[V](mapping: Mapping[Any, V]) -> frozenlist[V]:
    """Get the values of a map."""
    return tuple(mapping.values())


def empty() -> FrozenDict[Any, Any]:
    """Create an empty map."""
    return FrozenDict()


def filter[K, V](predicate: Callable[[V], bool], mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Filter a map based on values."""
    return FrozenDict({k: v for k, v in mapping.items() if predicate(v)})


def filter_with_key[K, V](
    predicate: Callable[[K, V], bool], mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Filter a map based on key-value pairs."""
    return FrozenDict({k: v for k, v in mapping.items() if predicate(k, v)})


def find_with_default[K, V](default: V, key: K, mapping: Mapping[K, V]) -> V:
    """Lookup a value with a default."""
    return mapping.get(key, default)


def from_list[K, V](pairs: Sequence[tuple[K, V]]) -> FrozenDict[K, V]:
    """Create a map from a list of key-value pairs."""
    return FrozenDict(dict(pairs))


def insert[K, V](key: K, value: V, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Insert a key-value pair into a map."""
    return FrozenDict({**mapping, key: value})


def keys[K](mapping: Mapping[K, Any]) -> frozenlist[K]:
    """Get the keys of a map."""
    return tuple(mapping.keys())


def lookup[K, V](key: K, mapping: Mapping[K, V]) -> Optional[V]:
    """Lookup a value in a map."""
    return mapping.get(key)


def map[K, V1, V2](f: Callable[[V1], V2], mapping: Mapping[K, V1]) -> FrozenDict[K, V2]:
    """Map a function over a map."""
    return FrozenDict[K, V2]({k: f(v) for k, v in mapping.items()})


def map_keys[K1, K2, V](
    f: Callable[[K1], K2], mapping: Mapping[K1, V]) -> FrozenDict[K2, V]:
    """Map a function over the keys of a map."""
    return FrozenDict({f(k): v for k, v in mapping.items()})


def member[K](key: K, mapping: Mapping[K, Any]) -> bool:
    """Check if a key is present in a map."""
    return key in mapping


def null(mapping: Mapping[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def remove[K, V](key: K, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Remove a key from a map."""
    return FrozenDict({k: v for k, v in mapping.items() if k != key})


def singleton[K, V](key: K, value: V) -> FrozenDict[K, V]:
    """Create a map with a single key-value pair."""
    return FrozenDict({key: value})


def size(mapping: Mapping[Any, Any]) -> int:
    """Get the size of a map."""
    return len(mapping)


def to_list[K, V](mapping: Mapping[K, V]) -> frozenlist[tuple[K, V]]:
    """Convert a map to a list of key-value pairs."""
    return tuple(mapping.items())


def union[K, V](map1: Mapping[K, V], map2: Mapping[K, V]) -> FrozenDict[K, V]:
    """Union two maps, with the second taking precedence."""
    return FrozenDict({**map1, **map2})

