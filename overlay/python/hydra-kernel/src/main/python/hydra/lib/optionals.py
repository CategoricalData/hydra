"""Python implementations of hydra.lib.optionals primitives."""

from __future__ import annotations
from collections.abc import Callable, Sequence
from typing import Any, TypeVar
from hydra.dsl.python import frozenlist, Optional, Given, None_, NONE_

A = TypeVar('A')
B = TypeVar('B')
C = TypeVar('C')


def apply(f: Optional[Callable[[A], B]], x: Optional[A]) -> Optional[B]:
    """Apply a function to an argument (applicative)."""
    match f:
        case None_():
            return NONE_
        case Given(func):
            match x:
                case None_():
                    return NONE_
                case Given(val):
                    return Given(func(val))


def bind(x: Optional[A], f: Callable[[A], Optional[B]]) -> Optional[B]:
    """Chain operations on optional values, handling Nothing cases automatically."""
    match x:
        case None_():
            return NONE_
        case Given(val):
            return f(val)


def cases(m: Optional[A], n: B | Callable[[], B], j: Callable[[A], B]) -> B:
    """Handle an optional value with the maybe value as the first argument."""
    match m:
        case None_():
            return n() if callable(n) else n  # type: ignore[return-value]
        case Given(val):
            return j(val)


def cat(xs: Sequence[Optional[A]]) -> frozenlist[A]:
    """Filter out Nothing values from a list."""
    result: list[A] = []
    for x in xs:
        match x:
            case Given(val):
                result.append(val)
            case None_():
                pass
    return tuple(result)


def compose(f: Callable[[A], Optional[B]], g: Callable[[B], Optional[C]], x: A) -> Optional[C]:
    """Compose two Maybe-returning functions (Kleisli composition)."""
    return bind(f(x), g)


def from_optional(default: A | Callable[[], A], x: Optional[A]) -> A:
    """Get a value from an optional value, or return a default value."""
    match x:
        case Given(val):
            return val
        case None_():
            return default() if callable(default) else default  # type: ignore[return-value]


def is_given(x: Optional[Any]) -> bool:
    """Check if a value is Just."""
    return isinstance(x, Given)


def is_none(x: Optional[Any]) -> bool:
    """Check if a value is Nothing."""
    return isinstance(x, None_)


def map(f: Callable[[A], B], x: Optional[A]) -> Optional[B]:
    """Map a function over an optional value."""
    match x:
        case Given(val):
            return Given(f(val))
        case None_():
            return NONE_


def map_optional(f: Callable[[A], Optional[B]], xs: Sequence[A]) -> frozenlist[B]:
    """Map a function over a list and collect Just results."""
    result: list[B] = []
    for x in xs:
        y = f(x)
        match y:
            case Given(val):
                result.append(val)
            case None_():
                pass
    return tuple(result)


def pure(x: A) -> Optional[A]:
    """Lift a value into the Maybe type."""
    return Given(x)


def to_list(x: Optional[A]) -> frozenlist[A]:
    """Convert a Maybe to a list: Just x becomes [x], Nothing becomes []."""
    match x:
        case Given(val):
            return (val,)
        case None_():
            return ()

