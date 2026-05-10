"""Phantom-typed term DSL for the hydra.lib.regex library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive2 as primitive2, lib_primitive3 as primitive3


def find(pattern: TTerm, s: TTerm) -> TTerm:
    """Find the first substring matching a regex pattern."""
    return primitive2(pattern, s)


def find_all(pattern: TTerm, s: TTerm) -> TTerm:
    """Find all non-overlapping substrings matching a regex pattern."""
    return primitive2(pattern, s)


def matches(pattern: TTerm, s: TTerm) -> TTerm:
    """Check whether an entire string matches a regex pattern."""
    return primitive2(pattern, s)


def replace(pattern: TTerm, replacement: TTerm, s: TTerm) -> TTerm:
    """Replace the first occurrence of a regex pattern with a replacement string."""
    return primitive3(pattern, replacement, s)


def replace_all(pattern: TTerm, replacement: TTerm, s: TTerm) -> TTerm:
    """Replace all non-overlapping occurrences of a regex pattern with a replacement string."""
    return primitive3(pattern, replacement, s)


def split(pattern: TTerm, s: TTerm) -> TTerm:
    """Split a string by a regex pattern."""
    return primitive2(pattern, s)
