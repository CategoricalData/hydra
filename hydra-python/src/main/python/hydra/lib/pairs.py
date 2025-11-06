"""Python implementations of hydra.lib.pairs primitives."""

from typing import TypeVar

A = TypeVar('A')
B = TypeVar('B')


def first[A, B](pair: tuple[A, B]) -> A:
    """Get the first element of a pair."""
    return pair[0]


def second[A, B](pair: tuple[A, B]) -> B:
    """Get the second element of a pair."""
    return pair[1]
