"""Python implementations of hydra.lib.maps primitives."""

from collections.abc import Callable, Mapping, Sequence
from typing import Any

from hydra.dsl.python import FrozenDict, frozenlist


def bimap[K1, K2, V1, V2](
    f: Callable[[K1], K2], g: Callable[[V1], V2], mapping: Mapping[K1, V1]
) -> FrozenDict[K2, V2]:
    """Map a function over the keys and values of a map."""
    return FrozenDict({f(k): g(v) for k, v in mapping.items()})


def empty[K, V]():
    """Create an empty map."""
    return FrozenDict[K, V]()


def from_list[K, V](pairs: Sequence[tuple[K, V]]) -> FrozenDict[K, V]:
    """Create a map from a list of key-value pairs."""
    return FrozenDict(dict(pairs))


def insert[K, V](key: K, value: V, mapping: Mapping[K, V]) -> FrozenDict[K, V]:
    """Insert a key-value pair into a map."""
    return FrozenDict({**mapping, key: value})


def is_empty(mapping: Mapping[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def keys[K](mapping: Mapping[K, Any]) -> frozenlist[K]:
    """Get the keys of a map."""
    return tuple(mapping.keys())


def lookup[K, V](key: K, mapping: Mapping[K, V]) -> V | None:
    """Lookup a value in a map."""
    return mapping.get(key)


def map[K, V1, V2](f: Callable[[V1], V2], mapping: Mapping[K, V1]) -> FrozenDict[K, V2]:
    """Map a function over a map."""
    return FrozenDict[K, V2]({k: f(v) for k, v in mapping.items()})


def map_keys[K1, K2, V](
    f: Callable[[K1], K2], mapping: Mapping[K1, V]
) -> FrozenDict[K2, V]:
    """Map a function over the keys of a map."""
    return FrozenDict({f(k): v for k, v in mapping.items()})


def remove[K](key: K, mapping: Mapping[K, Any]) -> FrozenDict[K, Any]:
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


def values[V](mapping: Mapping[Any, V]) -> frozenlist[V]:
    """Get the values of a map."""
    return tuple(mapping.values())
