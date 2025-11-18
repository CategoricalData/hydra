"""Phantom-typed term DSL for the hydra.lib.chars library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1


def is_alpha_num(c: TTerm) -> TTerm:
    """Check if a character is alphanumeric."""
    return primitive1(c)


def is_lower(c: TTerm) -> TTerm:
    """Check if a character is lowercase."""
    return primitive1(c)


def is_space(c: TTerm) -> TTerm:
    """Check if a character is whitespace."""
    return primitive1(c)


def is_upper(c: TTerm) -> TTerm:
    """Check if a character is uppercase."""
    return primitive1(c)


def to_lower(c: TTerm) -> TTerm:
    """Convert a character to lowercase."""
    return primitive1(c)


def to_upper(c: TTerm) -> TTerm:
    """Convert a character to uppercase."""
    return primitive1(c)
