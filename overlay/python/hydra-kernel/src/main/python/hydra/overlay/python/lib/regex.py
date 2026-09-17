"""Python implementations of hydra.lib.regex primitives.

Patterns are Hydra-defined and translingual (docs/specification/regex.md). Each primitive first runs
the pattern through hydra.parse.regex, then renders the AST to PCRE syntax via hydra.print.pcre.regex
(Python's re module is PCRE-like), before handing the rendered pattern to the native engine. An
ill-formed pattern (rejected by hydra.parse.regex) is treated as "no match" -- the same portable-
failure convention as an empty match. See issue #603.
"""

from __future__ import annotations

import re

import hydra.parse.regex
import hydra.print.pcre.regex
from hydra.overlay.python.dsl.python import Given, None_, Optional, NONE_, frozenlist


def _to_native(pattern: str) -> Optional[str]:
    """Translate a Hydra regex pattern to PCRE syntax. None_() if the pattern does not parse."""
    match hydra.parse.regex.parse_regex(pattern):
        case Given(value=ast):
            return Given(hydra.print.pcre.regex.print_regex(ast))
        case None_():
            return NONE_


def matches(pattern: str, input: str) -> bool:
    """Check whether an entire string matches a regex pattern."""
    match _to_native(pattern):
        case Given(value=native):
            return re.fullmatch(native, input) is not None
        case None_():
            return False


def find(pattern: str, input: str) -> Optional[str]:
    """Find the first substring matching a regex pattern."""
    match _to_native(pattern):
        case Given(value=native):
            m = re.search(native, input)
            if m is not None:
                return Given(m.group())
            return NONE_
        case None_():
            return NONE_


def find_all(pattern: str, input: str) -> frozenlist[str]:
    """Find all non-overlapping substrings matching a regex pattern."""
    match _to_native(pattern):
        case Given(value=native):
            return tuple(re.findall(native, input))
        case None_():
            return ()


def replace(pattern: str, replacement: str, input: str) -> str:
    """Replace the first occurrence of a regex pattern with a replacement string."""
    match _to_native(pattern):
        case Given(value=native):
            return re.sub(native, replacement, input, count=1)
        case None_():
            return input


def replace_all(pattern: str, replacement: str, input: str) -> str:
    """Replace all non-overlapping occurrences of a regex pattern with a replacement string."""
    match _to_native(pattern):
        case Given(value=native):
            return re.sub(native, replacement, input)
        case None_():
            return input


def split(pattern: str, input: str) -> frozenlist[str]:
    """Split a string by a regex pattern."""
    match _to_native(pattern):
        case Given(value=native):
            return tuple(re.split(native, input))
        case None_():
            return (input,)
