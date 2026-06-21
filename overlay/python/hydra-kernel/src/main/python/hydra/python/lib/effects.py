"""Python implementations of hydra.lib.effects primitives.

In Python the Hydra type ``effect<t>`` is transparent (the Python target lacks a
TypeVariantEffect), so ``effect<t>`` is just ``t``: effectful programs are ordinary
eager native code and "running the effect" simply means forcing the value. These
primitives therefore reduce to ordinary applications. See the Haskell reference
implementation in Hydra.Haskell.Lib.Effects (where effect<t> = IO t). For #494.
"""

from __future__ import annotations
from collections.abc import Callable, Sequence
from typing import TypeVar
from hydra.dsl.python import Optional, Given, None_, NONE_

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


def apply(f: Callable[[A], B], a: A) -> B:
    """Applicative apply for effects. Since effects are transparent, this just applies f to a."""
    return f(a)


def bind(a: A, f: Callable[[A], B]) -> B:
    """Sequence two effectful computations. Since effects are transparent, this just applies f to a."""
    return f(a)


def compose(f: Callable[[A], B], g: Callable[[B], C], a: A) -> C:
    """Kleisli composition for effects: run f, then g on its result."""
    return g(f(a))


def foldl(f: Callable[[A, B], A], acc: A, values: Sequence[B]) -> A:
    """Left-fold over a list with an effect-returning function."""
    result = acc
    for x in values:
        result = f(result, x)
    return result


def map(f: Callable[[A], B], a: A) -> B:
    """Map a pure function over the result of an effect. Since effects are transparent, just apply f."""
    return f(a)


def map_list(f: Callable[[A], B], values: Sequence[A]) -> tuple[B, ...]:
    """Map an effect-returning function over a list, collecting the results."""
    return tuple(f(x) for x in values)


def map_optional(f: Callable[[A], B], x: Optional[A]) -> Optional[B]:
    """Map an effect-returning function over an optional."""
    match x:
        case Given(val):
            return Given(f(val))
        case _:
            return NONE_


def pure(a: A) -> A:
    """Lift a pure value into an effect. Since effects are transparent, this is the identity."""
    return a
