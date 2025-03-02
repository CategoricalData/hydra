"""Python implementations of hydra.lib.maps primitives."""

from collections.abc import Callable
from typing import Any

from hydra.dsl.python import FrozenDict


def empty[K, V]():
    """Create an empty map."""
    return FrozenDict[K, V]()


def from_list[K, V](pairs: list[tuple[K, V]]) -> FrozenDict[K, V]:
    """Create a map from a list of key-value pairs."""
    return FrozenDict(dict(pairs))


def insert[K, V](key: K, value: V, mapping: FrozenDict[K, V]) -> FrozenDict[K, V]:
    """Insert a key-value pair into a map."""
    return mapping | {key: value}


def is_empty(mapping: dict[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def keys[K](mapping: dict[K, Any]) -> list[K]:
    """Get the keys of a map."""
    return list(mapping.keys())


def lookup[K, V](key: K, mapping: FrozenDict[K, V]) -> V | None:
    """Lookup a value in a map."""
    return mapping.get(key)


def map[K, V1, V2](
    f: Callable[[V1], V2], mapping: FrozenDict[K, V1]
) -> FrozenDict[K, V2]:
    """Map a function over a map."""
    return FrozenDict[K, V2]({k: f(v) for k, v in mapping.items()})


def map_keys[K1, K2, V](
    f: Callable[[K1], K2], mapping: FrozenDict[K1, V]
) -> FrozenDict[K2, V]:
    """Map a function over the keys of a map."""
    return FrozenDict[K2, V]({f(k): v for k, v in mapping.items()})


def remove[K](key: K, mapping: FrozenDict[K, Any]) -> FrozenDict[K, Any]:
    """Remove a key from a map."""
    return FrozenDict({k: v for k, v in mapping.items() if k != key})


def singleton[K, V](key: K, value: V) -> FrozenDict[K, V]:
    """Create a map with a single key-value pair."""
    return FrozenDict[K, V]({key: value})


def size(mapping: FrozenDict[Any, Any]) -> int:
    """Get the size of a map."""
    return len(mapping)


def to_list[K, V](mapping: FrozenDict[K, V]) -> list[tuple[K, V]]:
    """Convert a map to a list of key-value pairs."""
    return list(mapping.items())


def values[V](mapping: FrozenDict[Any, V]) -> list[V]:
    """Get the values of a map."""
    return list(mapping.values())
