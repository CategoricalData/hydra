"""Python implementations of hydra.lib.chars primitives."""

from __future__ import annotations


def is_alpha_num(value: int) -> bool:
    """Check whether a character is alphanumeric."""
    return chr(value).isalnum()


def is_lower(value: int) -> bool:
    """Check whether a character is lowercase."""
    return chr(value).islower()


def is_space(value: int) -> bool:
    """Check whether a character is a whitespace character."""
    return chr(value).isspace()


def is_upper(value: int) -> bool:
    """Check whether a character is uppercase."""
    return chr(value).isupper()


def to_lower(value: int) -> int:
    """Convert a character to lowercase."""
    return ord(chr(value).lower())


def to_upper(value: int) -> int:
    """Convert a character to uppercase."""
    return ord(chr(value).upper())
