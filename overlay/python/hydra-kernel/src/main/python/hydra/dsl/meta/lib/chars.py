"""Phantom-typed term DSL for the hydra.lib.chars library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1


def is_alpha_num(c: TypedTerm) -> TypedTerm:
    """Check if a character is alphanumeric."""
    return primitive1(c)


def is_lower(c: TypedTerm) -> TypedTerm:
    """Check if a character is lowercase."""
    return primitive1(c)


def is_space(c: TypedTerm) -> TypedTerm:
    """Check if a character is whitespace."""
    return primitive1(c)


def is_upper(c: TypedTerm) -> TypedTerm:
    """Check if a character is uppercase."""
    return primitive1(c)


def to_lower(c: TypedTerm) -> TypedTerm:
    """Convert a character to lowercase."""
    return primitive1(c)


def to_upper(c: TypedTerm) -> TypedTerm:
    """Convert a character to uppercase."""
    return primitive1(c)
