"""Phantom-typed term DSL for the hydra.lib.maps library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def alter(f: TTerm, key: TTerm, mapping: TTerm) -> TTerm:
    """Alter a value at a key using a function."""
    return primitive3(f, key, mapping)


def bimap(f: TTerm, g: TTerm, mapping: TTerm) -> TTerm:
    """Map a function over the keys and values of a map."""
    return primitive3(f, g, mapping)


def elems(mapping: TTerm) -> TTerm:
    """Get the values of a map."""
    return primitive1(mapping)


def empty() -> TTerm:
    """Create an empty map."""
    return primitive()


def filter(predicate: TTerm, mapping: TTerm) -> TTerm:
    """Filter a map based on values."""
    return primitive2(predicate, mapping)


def filter_with_key(predicate: TTerm, mapping: TTerm) -> TTerm:
    """Filter a map based on key-value pairs."""
    return primitive2(predicate, mapping)


def find_with_default(default: TTerm, key: TTerm, mapping: TTerm) -> TTerm:
    """Lookup a value with a default."""
    return primitive3(default, key, mapping)


def from_list(pairs: TTerm) -> TTerm:
    """Create a map from a list of key-value pairs."""
    return primitive1(pairs)


def insert(key: TTerm, value: TTerm, mapping: TTerm) -> TTerm:
    """Insert a key-value pair into a map."""
    return primitive3(key, value, mapping)


def keys(mapping: TTerm) -> TTerm:
    """Get the keys of a map."""
    return primitive1(mapping)


def lookup(key: TTerm, mapping: TTerm) -> TTerm:
    """Lookup a value in a map."""
    return primitive2(key, mapping)


def map(f: TTerm, mapping: TTerm) -> TTerm:
    """Map a function over a map."""
    return primitive2(f, mapping)


def map_keys(f: TTerm, mapping: TTerm) -> TTerm:
    """Map a function over the keys of a map."""
    return primitive2(f, mapping)


def member(key: TTerm, mapping: TTerm) -> TTerm:
    """Check if a key is present in a map."""
    return primitive2(key, mapping)


def null(mapping: TTerm) -> TTerm:
    """Check if a map is empty."""
    return primitive1(mapping)


def remove(key: TTerm, mapping: TTerm) -> TTerm:
    """Remove a key from a map."""
    return primitive2(key, mapping)


def singleton(key: TTerm, value: TTerm) -> TTerm:
    """Create a map with a single key-value pair."""
    return primitive2(key, value)


def size(mapping: TTerm) -> TTerm:
    """Get the size of a map."""
    return primitive1(mapping)


def to_list(mapping: TTerm) -> TTerm:
    """Convert a map to a list of key-value pairs."""
    return primitive1(mapping)


def union(map1: TTerm, map2: TTerm) -> TTerm:
    """Union two maps, with the first taking precedence."""
    return primitive2(map1, map2)
