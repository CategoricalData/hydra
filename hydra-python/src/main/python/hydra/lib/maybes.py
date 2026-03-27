"""Python implementations of hydra.lib.maybes primitives."""

from __future__ import annotations
from collections.abc import Callable, Sequence
from typing import Any, TypeVar
from hydra.dsl.python import frozenlist, Maybe, Just, Nothing, NOTHING

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def apply(f: Maybe[Callable[[A], B]], x: Maybe[A]) -> Maybe[B]:
    """Apply a function to an argument (applicative)."""
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


def cases(m: Maybe[A], n: B | Callable[[], B], j: Callable[[A], B]) -> B:
    """Handle an optional value with the maybe value as the first argument."""
    match m:
        case Nothing():
            return n() if callable(n) else n  # type: ignore[return-value]
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
    """Compose two Maybe-returning functions (Kleisli composition)."""
    return bind(f(x), g)


def from_just(x: Maybe[A]) -> A:
    """Extract value from a Just, or error on Nothing (partial function)."""
    match x:
        case Just(val):
            return val
        case Nothing():
            raise ValueError("from_just: Nothing")


def from_maybe(default: A | Callable[[], A], x: Maybe[A]) -> A:
    """Get a value from an optional value, or return a default value."""
    match x:
        case Just(val):
            return val
        case Nothing():
            return default() if callable(default) else default  # type: ignore[return-value]


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


def maybe(default: B | Callable[[], B], f: Callable[[A], B], x: Maybe[A]) -> B:
    """Eliminate an optional value with a default and a function."""
    match x:
        case Nothing():
            return default() if callable(default) else default  # type: ignore[return-value]
        case Just(val):
            return f(val)


def pure(x: A) -> Maybe[A]:
    """Lift a value into the Maybe type."""
    return Just(x)


def to_list(x: Maybe[A]) -> frozenlist[A]:
    """Convert a Maybe to a list: Just x becomes [x], Nothing becomes []."""
    match x:
        case Just(val):
            return (val,)
        case Nothing():
            return ()

