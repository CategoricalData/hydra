"""Python implementations of hydra.lib.maps primitives."""

from __future__ import annotations
from collections.abc import Callable, Mapping, Sequence
from functools import cmp_to_key
from typing import Any, TypeVar

from hydra.dsl.python import FrozenDict, frozenlist, Maybe, Just, Nothing, NOTHING

K = TypeVar('K')
K1 = TypeVar('K1')
K2 = TypeVar('K2')
V = TypeVar('V')
V1 = TypeVar('V1')
V2 = TypeVar('V2')


def _structural_compare(x: Any, y: Any) -> int:
    """Compare two values structurally, like Haskell's derived Ord.

    Returns -1 if x < y, 0 if x == y, 1 if x > y.
    This provides a total ordering for complex types like Term.
    """
    # Try native comparison first (for simple types like int, str, etc.)
    try:
        if x < y:
            return -1
        elif x > y:
            return 1
        else:
            return 0
    except TypeError:
        pass

    # For complex types, compare structurally
    tx, ty = type(x), type(y)

    # Different types: compare by type name
    if tx != ty:
        return -1 if tx.__name__ < ty.__name__ else 1

    # Same type: compare by structure
    # Handle dataclasses and similar objects with __dict__
    if hasattr(x, '__dict__'):
        x_items = sorted(x.__dict__.items())
        y_items = sorted(y.__dict__.items())
        for (kx, vx), (ky, vy) in zip(x_items, y_items):
            if kx != ky:
                return -1 if kx < ky else 1
            result = _structural_compare(vx, vy)
            if result != 0:
                return result
        return _structural_compare(len(x_items), len(y_items))

    # Handle tuples and lists
    if isinstance(x, (tuple, list)):
        for xi, yi in zip(x, y):
            result = _structural_compare(xi, yi)
            if result != 0:
                return result
        return _structural_compare(len(x), len(y))

    # Handle dicts/mappings
    if isinstance(x, Mapping):
        x_items = sorted(x.items(), key=cmp_to_key(lambda a, b: _structural_compare(a, b)))
        y_items = sorted(y.items(), key=cmp_to_key(lambda a, b: _structural_compare(a, b)))
        for (kx, vx), (ky, vy) in zip(x_items, y_items):
            result = _structural_compare(kx, ky)
            if result != 0:
                return result
            result = _structural_compare(vx, vy)
            if result != 0:
                return result
        return _structural_compare(len(x_items), len(y_items))

    # Fallback: compare by repr
    rx, ry = repr(x), repr(y)
    if rx < ry:
        return -1
    elif rx > ry:
        return 1
    return 0


def _sorted_by_key(items: Any) -> list[Any]:
    """Sort items using structural comparison."""
    return sorted(items, key=cmp_to_key(lambda a, b: _structural_compare(a[0], b[0])))


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
    return tuple(v for _, v in _sorted_by_key(mapping.items()))


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
    return tuple(k for k, _ in _sorted_by_key((k, None) for k in mapping.keys()))


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
    return tuple(_sorted_by_key(mapping.items()))


def union(map1: Mapping[K, V], map2: Mapping[K, V]) -> FrozenDict[K, V]:
    """Union two maps, with the first taking precedence."""
    return FrozenDict({**map2, **map1})
