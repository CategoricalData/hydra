"""Primitive functions for the Hydra strings DSL."""

from hydra.dsl.python import frozenlist
from hydra.tools import primitive


@primitive()
def to_lower(s: str) -> str:
    """Convert a string to lowercase."""
    return s.lower()

@primitive()
def to_upper(s: str) -> str:
    """Convert a string to uppercase."""
    return s.upper()


@primitive()
def length(s: str) -> int:
    """Get the length of a string."""
    return len(s)


@primitive()
def cat(strings: frozenlist[str]) -> str:
    """Concatenate a list of strings."""
    return "".join(strings)


# TODO: Get primitive to handle multiple intputs
def split_on(delimiter: str, x: str) -> frozenlist[str]:
    """Split a string on a delimiter."""
    return tuple(x.split(delimiter))
