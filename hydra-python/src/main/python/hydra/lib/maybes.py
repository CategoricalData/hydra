"""Python implementations of hydra.lib.maybes primitives."""

from __future__ import annotations
from collections.abc import Callable, Sequence
from typing import Any, TypeVar
from hydra.dsl.python import frozenlist, Maybe, Just, Nothing, NOTHING

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def apply(f: Maybe[Callable[[A], B]], x: Maybe[A]) -> Maybe[B]:
    """Apply a function to an argument."""
    match f:
        case Nothing():
            return NOTHING
        case Just(func):
            match x:
                case Nothing():
                    return NOTHING
                case Just(val):
                    return Just(func(val))


def bind(x: Maybe[A], f: Callable[[A], Maybe[B]]) -> Maybe[B]:
    """Chain operations on optional values, handling Nothing cases automatically."""
    match x:
        case Nothing():
            return NOTHING
        case Just(val):
            return f(val)


def cases(m: Maybe[A], n: B, j: Callable[[A], B]) -> B:
    """Handle an optional value with different parameter order than maybe."""
    match m:
        case Nothing():
            return n
        case Just(val):
            return j(val)


def cat(xs: Sequence[Maybe[A]]) -> frozenlist[A]:
    """Filter out Nothing values from a list."""
    result: list[A] = []
    for x in xs:
        match x:
            case Just(val):
                result.append(val)
            case Nothing():
                pass
    return tuple(result)


def compose(f: Callable[[A], Maybe[B]], g: Callable[[B], Maybe[C]], x: A) -> Maybe[C]:
    """Compose two Maybe-returning functions and apply to a value.

    This is Kleisli composition: (f >=> g) x = f x >>= g
    """
    return bind(f(x), g)


def from_just(x: Maybe[A]) -> A:
    """Extract value from Maybe, assuming it's Just (unsafe)."""
    match x:
        case Just(val):
            return val
        case Nothing():
            raise ValueError("from_just: Nothing")


def from_maybe(default: A, x: Maybe[A]) -> A:
    """Get a value from an optional value, or return a default value."""
    match x:
        case Just(val):
            return val
        case Nothing():
            return default


def is_just(x: Maybe[Any]) -> bool:
    """Check if a value is Just."""
    return isinstance(x, Just)


def is_nothing(x: Maybe[Any]) -> bool:
    """Check if a value is Nothing."""
    return isinstance(x, Nothing)


def map(f: Callable[[A], B], x: Maybe[A]) -> Maybe[B]:
    """Map a function over an optional value."""
    match x:
        case Just(val):
            return Just(f(val))
        case Nothing():
            return NOTHING


def map_maybe(f: Callable[[A], Maybe[B]], xs: Sequence[A]) -> frozenlist[B]:
    """Map a function over a list and collect Just results."""
    result: list[B] = []
    for x in xs:
        y = f(x)
        match y:
            case Just(val):
                result.append(val)
            case Nothing():
                pass
    return tuple(result)


def maybe(default: B, f: Callable[[A], B], x: Maybe[A]) -> B:
    """Handle an optional value, with transformation."""
    match x:
        case Nothing():
            return default
        case Just(val):
            return f(val)


def pure(x: A) -> Maybe[A]:
    """Lift a value into the Maybe type."""
    return Just(x)

