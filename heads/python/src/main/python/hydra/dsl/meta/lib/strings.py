"""Phantom-typed term DSL for the hydra.lib.strings library."""

from typing import Sequence

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import (
    lib_primitive1 as primitive1,
    lib_primitive2 as primitive2,
    list_,
)


def cat(strings: TypedTerm) -> TypedTerm:
    """Concatenate a list of strings."""
    return primitive1(strings)


def cat2(s1: TypedTerm, s2: TypedTerm) -> TypedTerm:
    """Concatenate two strings."""
    return primitive2(s1, s2)


def concat(strings: Sequence[TypedTerm]) -> TypedTerm:
    """Concatenate a Python sequence of string terms into a single string."""
    return cat(list_(strings))


def from_list(chars: TypedTerm) -> TypedTerm:
    """Convert a list of character codes to a string."""
    return primitive1(chars)


def intercalate(separator: TypedTerm, strings: TypedTerm) -> TypedTerm:
    """Insert a separator between strings and concatenate."""
    return primitive2(separator, strings)


def length(s: TypedTerm) -> TypedTerm:
    """Get the length of a string."""
    return primitive1(s)


def lines(s: TypedTerm) -> TypedTerm:
    """Split a string into lines."""
    return primitive1(s)


def maybe_char_at(index: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Get the character at a given index in a string, returning Nothing if out of bounds."""
    return primitive2(index, s)


def null(s: TypedTerm) -> TypedTerm:
    """Check if a string is empty."""
    return primitive1(s)


def split_on(separator: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Split a string on a separator."""
    return primitive2(separator, s)


def to_list(s: TypedTerm) -> TypedTerm:
    """Convert a string to a list of character codes."""
    return primitive1(s)


def to_lower(s: TypedTerm) -> TypedTerm:
    """Convert a string to lowercase."""
    return primitive1(s)


def to_upper(s: TypedTerm) -> TypedTerm:
    """Convert a string to uppercase."""
    return primitive1(s)


def unlines(strings: TypedTerm) -> TypedTerm:
    """Join lines with newline characters."""
    return primitive1(strings)
