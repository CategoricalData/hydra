"""Phantom-typed term DSL for the hydra.lib.strings library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive2 as primitive2


def cat(strings: TTerm) -> TTerm:
    """Concatenate a list of strings."""
    return primitive1(strings)


def cat2(s1: TTerm, s2: TTerm) -> TTerm:
    """Concatenate two strings."""
    return primitive2(s1, s2)


def char_at(index: TTerm, s: TTerm) -> TTerm:
    """Get the character at a given index in a string."""
    return primitive2(index, s)


def from_list(chars: TTerm) -> TTerm:
    """Convert a list of character codes to a string."""
    return primitive1(chars)


def intercalate(separator: TTerm, strings: TTerm) -> TTerm:
    """Insert a separator between strings and concatenate."""
    return primitive2(separator, strings)


def length(s: TTerm) -> TTerm:
    """Get the length of a string."""
    return primitive1(s)


def lines(s: TTerm) -> TTerm:
    """Split a string into lines."""
    return primitive1(s)


def null(s: TTerm) -> TTerm:
    """Check if a string is empty."""
    return primitive1(s)


def split_on(separator: TTerm, s: TTerm) -> TTerm:
    """Split a string on a separator."""
    return primitive2(separator, s)


def to_list(s: TTerm) -> TTerm:
    """Convert a string to a list of character codes."""
    return primitive1(s)


def to_lower(s: TTerm) -> TTerm:
    """Convert a string to lowercase."""
    return primitive1(s)


def to_upper(s: TTerm) -> TTerm:
    """Convert a string to uppercase."""
    return primitive1(s)


def unlines(strings: TTerm) -> TTerm:
    """Join lines with newline characters."""
    return primitive1(strings)
