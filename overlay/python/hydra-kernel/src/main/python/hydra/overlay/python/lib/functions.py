"""Python implementations of hydra.lib.functions primitives."""

from __future__ import annotations
from typing import TypeVar

A = TypeVar('A')
B = TypeVar('B')


def absurd(v: A) -> B:
    """Eliminate a value of the uninhabited void type. Unreachable in any well-typed program."""
    raise AssertionError("hydra.lib.functions.absurd: void has no inhabitants")


def identity(a: A) -> A:
    """Return a value unchanged."""
    return a
