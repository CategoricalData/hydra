"""Phantom-typed term DSL for the hydra.lib.lists library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def apply(fs: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Apply a list of functions to a list of values (applicative style)."""
    return primitive2(fs, values)


def bind(values: TypedTerm, f: TypedTerm) -> TypedTerm:
    """Apply a function that returns lists to each element and flatten results."""
    return primitive2(values, f)


def concat(values: TypedTerm) -> TypedTerm:
    """Concatenate a list of lists."""
    return primitive1(values)


def concat2(values1: TypedTerm, values2: TypedTerm) -> TypedTerm:
    """Concatenate two lists."""
    return primitive2(values1, values2)


def cons(value: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Prepend a value to a list."""
    return primitive2(value, values)


def drop(n: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Drop the first n elements from a list."""
    return primitive2(n, values)


def drop_while(predicate: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Drop elements from the beginning of a list while predicate is true."""
    return primitive2(predicate, values)


def elem(value: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Check if an element is in a list."""
    return primitive2(value, values)


def filter(f: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Filter a list based on a predicate."""
    return primitive2(f, values)


def find(predicate: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Find the first element matching a predicate, returning none if none matches."""
    return primitive2(predicate, values)


def foldl(f: TypedTerm, initial: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Fold a list from the left."""
    return primitive3(f, initial, values)


def foldr(f: TypedTerm, initial: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Fold a list from the right."""
    return primitive3(f, initial, values)


def group(values: TypedTerm) -> TypedTerm:
    """Group consecutive equal elements."""
    return primitive1(values)


def intercalate(separator: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Intercalate a list of lists."""
    return primitive2(separator, values)


def intersperse(separator: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Intersperse a value between elements of a list."""
    return primitive2(separator, values)


def length(values: TypedTerm) -> TypedTerm:
    """Get the length of a list."""
    return primitive1(values)


def map(f: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Map a function over a list."""
    return primitive2(f, values)


def maybe_at(i: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Get the element at a specified index, returning none if out of bounds."""
    return primitive2(i, values)


def maybe_head(values: TypedTerm) -> TypedTerm:
    """Get the first element of a list, returning none if the list is empty."""
    return primitive1(values)


def maybe_init(values: TypedTerm) -> TypedTerm:
    """Return all elements except the last, returning none if the list is empty."""
    return primitive1(values)


def maybe_last(values: TypedTerm) -> TypedTerm:
    """Get the last element of a list, returning none if the list is empty."""
    return primitive1(values)


def maybe_tail(values: TypedTerm) -> TypedTerm:
    """Get all elements except the first, returning none if the list is empty."""
    return primitive1(values)


def nub(values: TypedTerm) -> TypedTerm:
    """Remove duplicate elements from a list."""
    return primitive1(values)


def null(values: TypedTerm) -> TypedTerm:
    """Check if a list is empty."""
    return primitive1(values)


def partition(predicate: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Partition a list into elements that satisfy a predicate and elements that do not."""
    return primitive2(predicate, values)


def pure(value: TypedTerm) -> TypedTerm:
    """Create a list with a single element."""
    return primitive1(value)


def replicate(n: TypedTerm, value: TypedTerm) -> TypedTerm:
    """Create a list with n copies of a value."""
    return primitive2(n, value)


def reverse(values: TypedTerm) -> TypedTerm:
    """Reverse a list."""
    return primitive1(values)


def singleton(value: TypedTerm) -> TypedTerm:
    """Create a single-element list."""
    return primitive1(value)


def sort(values: TypedTerm) -> TypedTerm:
    """Sort a list."""
    return primitive1(values)


def sort_on(key: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Sort a list based on a key function."""
    return primitive2(key, values)


def span(predicate: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Split a list at the first element where predicate fails."""
    return primitive2(predicate, values)


def take(n: TypedTerm, values: TypedTerm) -> TypedTerm:
    """Take the first n elements from a list."""
    return primitive2(n, values)


def transpose(values: TypedTerm) -> TypedTerm:
    """Transpose a list of lists."""
    return primitive1(values)


def uncons(values: TypedTerm) -> TypedTerm:
    """Decompose a list into its head and tail, returning none if the list is empty."""
    return primitive1(values)


def zip(values1: TypedTerm, values2: TypedTerm) -> TypedTerm:
    """Zip two lists into pairs."""
    return primitive2(values1, values2)


def zip_with(f: TypedTerm, values1: TypedTerm, values2: TypedTerm) -> TypedTerm:
    """Zip two lists with a combining function."""
    return primitive3(f, values1, values2)
