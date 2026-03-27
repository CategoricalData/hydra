"""Python implementations of hydra.lib.regex primitives."""

from __future__ import annotations

import re

from hydra.dsl.python import Just, Maybe, NOTHING, frozenlist


def find(pattern: str, input: str) -> Maybe[str]:
    """Find the first substring matching a regex pattern."""
    m = re.search(pattern, input)
    if m is not None:
        return Just(m.group())
    return NOTHING


def find_all(pattern: str, input: str) -> frozenlist[str]:
    """Find all non-overlapping substrings matching a regex pattern."""
    return tuple(re.findall(pattern, input))


def matches(pattern: str, input: str) -> bool:
    """Check whether an entire string matches a regex pattern."""
    return re.fullmatch(pattern, input) is not None


def replace(pattern: str, replacement: str, input: str) -> str:
    """Replace the first occurrence of a regex pattern with a replacement string."""
    return re.sub(pattern, replacement, input, count=1)


def replace_all(pattern: str, replacement: str, input: str) -> str:
    """Replace all non-overlapping occurrences of a regex pattern with a replacement string."""
    return re.sub(pattern, replacement, input)


def split(pattern: str, input: str) -> frozenlist[str]:
    """Split a string by a regex pattern."""
    return tuple(re.split(pattern, input))
