"""Python implementations of hydra.lib.tuples primitives."""

from collections.abc import Callable
from typing import TypeVar

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def curry[A, B, C](f: Callable[[tuple[A, B]], C]) -> Callable[[A], Callable[[B], C]]:
    """Convert a function that takes a pair to a curried function."""
    return lambda x: lambda y: f((x, y))


def fst[A, B](pair: tuple[A, B]) -> A:
    """Get the first element of a pair."""
    return pair[0]


def snd[A, B](pair: tuple[A, B]) -> B:
    """Get the second element of a pair."""
    return pair[1]


def uncurry[A, B, C](f: Callable[[A], Callable[[B], C]]) -> Callable[[tuple[A, B]], C]:
    """Convert a curried function to a function that takes a pair."""
    return lambda pair: f(pair[0])(pair[1])
