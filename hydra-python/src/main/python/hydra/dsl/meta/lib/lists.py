"""Phantom-typed term DSL for the hydra.lib.lists library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def apply(fs: TTerm, values: TTerm) -> TTerm:
    """Apply a list of functions to a list of values (applicative style)."""
    return primitive2(fs, values)


def at(i: TTerm, values: TTerm) -> TTerm:
    """Get the element at specified index of a list."""
    return primitive2(i, values)


def bind(values: TTerm, f: TTerm) -> TTerm:
    """Apply a function that returns lists to each element and flatten results."""
    return primitive2(values, f)


def concat(values: TTerm) -> TTerm:
    """Concatenate a list of lists."""
    return primitive1(values)


def concat2(values1: TTerm, values2: TTerm) -> TTerm:
    """Concatenate two lists."""
    return primitive2(values1, values2)


def cons(value: TTerm, values: TTerm) -> TTerm:
    """Prepend a value to a list."""
    return primitive2(value, values)


def drop(n: TTerm, values: TTerm) -> TTerm:
    """Drop the first n elements from a list."""
    return primitive2(n, values)


def drop_while(predicate: TTerm, values: TTerm) -> TTerm:
    """Drop elements from the beginning of a list while predicate is true."""
    return primitive2(predicate, values)


def elem(value: TTerm, values: TTerm) -> TTerm:
    """Check if an element is in a list."""
    return primitive2(value, values)


def filter(f: TTerm, values: TTerm) -> TTerm:
    """Filter a list based on a predicate."""
    return primitive2(f, values)


def foldl(f: TTerm, initial: TTerm, values: TTerm) -> TTerm:
    """Fold a list from the left."""
    return primitive3(f, initial, values)


def group(values: TTerm) -> TTerm:
    """Group consecutive equal elements."""
    return primitive1(values)


def head(values: TTerm) -> TTerm:
    """Get the first element of a list."""
    return primitive1(values)


def init(values: TTerm) -> TTerm:
    """Return all elements except the last one."""
    return primitive1(values)


def intercalate(separator: TTerm, values: TTerm) -> TTerm:
    """Intercalate a list of lists."""
    return primitive2(separator, values)


def intersperse(separator: TTerm, values: TTerm) -> TTerm:
    """Intersperse a value between elements of a list."""
    return primitive2(separator, values)


def last(values: TTerm) -> TTerm:
    """Get the last element of a list."""
    return primitive1(values)


def length(values: TTerm) -> TTerm:
    """Get the length of a list."""
    return primitive1(values)


def map(f: TTerm, values: TTerm) -> TTerm:
    """Map a function over a list."""
    return primitive2(f, values)


def nub(values: TTerm) -> TTerm:
    """Remove duplicate elements from a list."""
    return primitive1(values)


def null(values: TTerm) -> TTerm:
    """Check if a list is empty."""
    return primitive1(values)


def pure(value: TTerm) -> TTerm:
    """Create a list with a single element."""
    return primitive1(value)


def replicate(n: TTerm, value: TTerm) -> TTerm:
    """Create a list with n copies of a value."""
    return primitive2(n, value)


def reverse(values: TTerm) -> TTerm:
    """Reverse a list."""
    return primitive1(values)


def safe_head(values: TTerm) -> TTerm:
    """Get the first element of a list, returning None if the list is empty."""
    return primitive1(values)


def singleton(value: TTerm) -> TTerm:
    """Create a single-element list."""
    return primitive1(value)


def sort(values: TTerm) -> TTerm:
    """Sort a list."""
    return primitive1(values)


def sort_on(key: TTerm, values: TTerm) -> TTerm:
    """Sort a list based on a key function."""
    return primitive2(key, values)


def span(predicate: TTerm, values: TTerm) -> TTerm:
    """Split a list at the first element where predicate fails."""
    return primitive2(predicate, values)


def tail(values: TTerm) -> TTerm:
    """Get all elements of a list except the first."""
    return primitive1(values)


def take(n: TTerm, values: TTerm) -> TTerm:
    """Take the first n elements from a list."""
    return primitive2(n, values)


def transpose(values: TTerm) -> TTerm:
    """Transpose a list of lists."""
    return primitive1(values)


def zip(values1: TTerm, values2: TTerm) -> TTerm:
    """Zip two lists into pairs."""
    return primitive2(values1, values2)


def zip_with(f: TTerm, values1: TTerm, values2: TTerm) -> TTerm:
    """Zip two lists with a combining function."""
    return primitive3(f, values1, values2)
