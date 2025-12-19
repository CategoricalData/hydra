"""Python implementations of hydra.lib.pairs primitives."""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')
D = TypeVar('D')


def bimap(f: Callable[[A], C], g: Callable[[B], D], pair: tuple[A, B]) -> tuple[C, D]:
    """Map over both elements of a pair."""
    return (f(pair[0]), g(pair[1]))


def first(pair: tuple[A, B]) -> A:
    """Get the first element of a pair."""
    return pair[0]


def second(pair: tuple[A, B]) -> B:
    """Get the second element of a pair."""
    return pair[1]
