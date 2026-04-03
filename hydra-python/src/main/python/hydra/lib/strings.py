"""Python implementations of hydra.lib.strings primitives."""

from __future__ import annotations
from collections.abc import Sequence

from hydra.dsl.python import frozenlist, Maybe, Just, Nothing


def cat(xs: Sequence[str]) -> str:
    """Concatenate a list of strings into a single string."""
    return "".join(xs)


def cat2(s1: str, s2: str) -> str:
    """Concatenate two strings."""
    return s1 + s2


def char_at(i: int, s: str) -> int:
    """Get the Unicode code point of the character at a specific index in a string."""
    return ord(s[i])


def from_list(values: Sequence[int]) -> str:
    """Convert a list of Unicode code points to a string."""
    return "".join(chr(v) for v in values)


def intercalate(separator: str, values: Sequence[str]) -> str:
    """Join a list of strings with a separator between each element."""
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


def maybe_char_at(i: int, s: str) -> Maybe[int]:
    """Get the Unicode code point at a specific index, returning Nothing if out of bounds."""
    return Just(ord(s[i])) if 0 <= i < len(s) else Nothing()


def null(s: str) -> bool:
    """Check whether a string is empty."""
    return len(s) == 0


def split_on(delimiter: str, x: str) -> frozenlist[str]:
    """Split a string on a delimiter string."""
    if not delimiter:
        # Haskell: splitOn "" "abc" == ["", "a", "b", "c"]
        # splitOn "" "" == [""]
        if not x:
            return ("",)
        return ("",) + tuple(x)
    return tuple(x.split(delimiter))


def to_list(x: str) -> frozenlist[int]:
    """Convert a string to a list of Unicode code points."""
    return tuple(ord(c) for c in x)


def to_lower(s: str) -> str:
    """Convert a string to lowercase."""
    return s.lower()


def to_upper(s: str) -> str:
    """Convert a string to uppercase."""
    return s.upper()


def unlines(xs: Sequence[str]) -> str:
    """Join a list of strings with newlines, appending a trailing newline."""
    if not xs:
        return ""
    return '\n'.join(xs) + '\n'


def words(s: str) -> frozenlist[str]:
    """Split a string into words."""
    return tuple(s.split())
