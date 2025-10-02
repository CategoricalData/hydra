"""Python implementations of hydra.lib.strings primitives."""

from collections.abc import Sequence

from hydra.dsl.python import frozenlist


def cat(xs: Sequence[str]) -> str:
    """Concatenate a list of strings."""
    return "".join(xs)


def cat2(s1: str, s2: str) -> str:
    """Concatenate two strings."""
    return s1 + s2


def char_at(i: int, s: str) -> int:
    """Get the character code at a specific index in a string."""
    return ord(s[i])


def from_list(values: Sequence[int]) -> str:
    """Convert a list of integers to a string."""
    return "".join(chr(v) for v in values)


def intercalate(separator: str, values: Sequence[str]) -> str:
    """Intercalate a string between a list of strings."""
    return separator.join(values)


def length(s: str) -> int:
    """Return the length of a string."""
    return len(s)


def lines(s: str) -> frozenlist[str]:
    """Split a string into lines."""
    if not s:
        return ()
    result = s.split('\n')
    # Remove trailing empty string if the string ends with newline
    if result and result[-1] == '':
        result = result[:-1]
    return tuple(result)


def null(s: str) -> bool:
    """Check if a string is null/empty."""
    return len(s) == 0


def split_on(delimiter: str, x: str) -> frozenlist[str]:
    """Split a string on a delimiter."""
    return tuple(x.split(delimiter))


def to_list(x: str) -> frozenlist[int]:
    """Convert a string to a list of integers."""
    return tuple(ord(c) for c in x)


def to_lower(s: str) -> str:
    """Convert a string to lowercase."""
    return s.lower()


def to_upper(s: str) -> str:
    """Convert a string to uppercase."""
    return s.upper()


def unlines(xs: Sequence[str]) -> str:
    """Join strings with newlines."""
    return '\n'.join(xs)
