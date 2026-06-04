"""Phantom-typed term DSL for the hydra.lib.regex library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive2 as primitive2, lib_primitive3 as primitive3


def find(pattern: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Find the first substring matching a regex pattern."""
    return primitive2(pattern, s)


def find_all(pattern: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Find all non-overlapping substrings matching a regex pattern."""
    return primitive2(pattern, s)


def matches(pattern: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Check whether an entire string matches a regex pattern."""
    return primitive2(pattern, s)


def replace(pattern: TypedTerm, replacement: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Replace the first occurrence of a regex pattern with a replacement string."""
    return primitive3(pattern, replacement, s)


def replace_all(pattern: TypedTerm, replacement: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Replace all non-overlapping occurrences of a regex pattern with a replacement string."""
    return primitive3(pattern, replacement, s)


def split(pattern: TypedTerm, s: TypedTerm) -> TypedTerm:
    """Split a string by a regex pattern."""
    return primitive2(pattern, s)
