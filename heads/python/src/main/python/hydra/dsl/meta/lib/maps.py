"""Phantom-typed term DSL for the hydra.lib.maps library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def alter(f: TypedTerm, key: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Alter a value at a key using a function."""
    return primitive3(f, key, mapping)


def bimap(f: TypedTerm, g: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Map a function over the keys and values of a map."""
    return primitive3(f, g, mapping)


def delete(key: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Remove a key from a map."""
    return primitive2(key, mapping)


def elems(mapping: TypedTerm) -> TypedTerm:
    """Get the values of a map."""
    return primitive1(mapping)


def empty() -> TypedTerm:
    """Create an empty map."""
    return primitive()


def filter(predicate: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Filter a map based on values."""
    return primitive2(predicate, mapping)


def filter_with_key(predicate: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Filter a map based on key-value pairs."""
    return primitive2(predicate, mapping)


def find_with_default(default: TypedTerm, key: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Lookup a value with a default."""
    return primitive3(default, key, mapping)


def from_list(pairs: TypedTerm) -> TypedTerm:
    """Create a map from a list of key-value pairs."""
    return primitive1(pairs)


def insert(key: TypedTerm, value: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Insert a key-value pair into a map."""
    return primitive3(key, value, mapping)


def keys(mapping: TypedTerm) -> TypedTerm:
    """Get the keys of a map."""
    return primitive1(mapping)


def lookup(key: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Lookup a value in a map."""
    return primitive2(key, mapping)


def map(f: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Map a function over a map."""
    return primitive2(f, mapping)


def map_keys(f: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Map a function over the keys of a map."""
    return primitive2(f, mapping)


def member(key: TypedTerm, mapping: TypedTerm) -> TypedTerm:
    """Check if a key is present in a map."""
    return primitive2(key, mapping)


def null(mapping: TypedTerm) -> TypedTerm:
    """Check if a map is empty."""
    return primitive1(mapping)


def singleton(key: TypedTerm, value: TypedTerm) -> TypedTerm:
    """Create a map with a single key-value pair."""
    return primitive2(key, value)


def size(mapping: TypedTerm) -> TypedTerm:
    """Get the size of a map."""
    return primitive1(mapping)


def to_list(mapping: TypedTerm) -> TypedTerm:
    """Convert a map to a list of key-value pairs."""
    return primitive1(mapping)


def union(map1: TypedTerm, map2: TypedTerm) -> TypedTerm:
    """Union two maps, with the first taking precedence."""
    return primitive2(map1, map2)
