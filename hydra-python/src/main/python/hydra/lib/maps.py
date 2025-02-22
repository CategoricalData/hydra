"""Python implementations of hydra.lib.maps primitives."""

from collections.abc import Callable
from typing import Any


def empty[K, V]():
    """Create an empty map."""
    return dict[K, V]()


def from_list[K, V](pairs: list[tuple[K, V]]) -> dict[K, V]:
    """Create a map from a list of key-value pairs."""
    return dict(pairs)


def insert[K, V](key: K, value: V, mapping: dict[K, V]) -> dict[K, V]:
    """Insert a key-value pair into a map."""
    return mapping | {key: value}


def is_empty(mapping: dict[Any, Any]) -> bool:
    """Check if a map is empty."""
    return len(mapping) == 0


def keys[K](mapping: dict[K, Any]) -> list[K]:
    """Get the keys of a map."""
    return list(mapping.keys())


def lookup[K, V](key: K, mapping: dict[K, V]) -> V | None:
    """Lookup a value in a map."""
    return mapping.get(key)


def map[K, V1, V2](f: Callable[[V1], V2], mapping: dict[K, V1]) -> dict[K, V2]:
    """Map a function over a map."""
    return {k: f(v) for k, v in mapping.items()}


def map_keys[K1, K2, V](f: Callable[[K1], K2], mapping: dict[K1, V]) -> dict[K2, V]:
    """Map a function over the keys of a map."""
    return {f(k): v for k, v in mapping.items()}


def remove[K](key: K, mapping: dict[K, Any]) -> dict[K, Any]:
    """Remove a key from a map."""
    result = mapping.copy()
    if key in result:
        del result[key]
    return result


def singleton[K, V](key: K, value: V) -> dict[K, V]:
    """Create a map with a single key-value pair."""
    return {key: value}


def size(mapping: dict[Any, Any]) -> int:
    """Get the size of a map."""
    return len(mapping)


def to_list[K, V](mapping: dict[K, V]) -> list[tuple[K, V]]:
    """Convert a map to a list of key-value pairs."""
    return list(mapping.items())


def values[V](mapping: dict[Any, V]) -> list[V]:
    """Get the values of a map."""
    return list(mapping.values())
