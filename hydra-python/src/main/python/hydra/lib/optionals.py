"""Python implementations of hydra.lib.optionals primitives."""

from collections.abc import Callable, Sequence
from typing import Any
from hydra.dsl.python import frozenlist


def apply[A, B](f: Callable[[A], B] | None, x: A | None) -> B | None:
    """Apply a function to an argument."""
    if f is None or x is None:
        return None
    return f(x)


def bind[A, B](x: A | None, f: Callable[[A], B | None]) -> B | None:
    """Chain operations on optional values, handling None cases automatically."""
    if x is None:
        return None
    return f(x)


def cases[A, B](m: A | None, n: B, j: Callable[[A], B]) -> B:
    """Handle an optional value with different parameter order than maybe."""
    return n if m is None else j(m)


def cat[A](xs: Sequence[A | None]) -> frozenlist[A]:
    """Filter out None values from a list."""
    return tuple(x for x in xs if x is not None)


def compose[A, B, C](
    f: Callable[[A], B | None], g: Callable[[B], C | None]
) -> Callable[[A], C | None]:
    """Compose two functions."""
    return lambda x: bind(f(x), g)


def from_just[A](x: A | None) -> A:
    """Extract value from Maybe, assuming it's not None (unsafe)."""
    if x is None:
        raise ValueError("from_just: None")
    return x


def from_maybe[A](default: A, x: A | None) -> A:
    """Get a value from an optional value, or return a default value."""
    return x if x is not None else default


def is_just(x: Any | None) -> bool:
    """Check if a value is not None."""
    return x is not None


def is_nothing(x: Any | None) -> bool:
    """Check if a value is None."""
    return x is None


def map[A, B](f: Callable[[A], B], x: A | None) -> B | None:
    """Map a function over an optional value."""
    return f(x) if x is not None else None


def map_maybe[A, B](f: Callable[[A], B | None], xs: Sequence[A]) -> frozenlist[B]:
    """Map a function over a list and collect non-None results."""
    result: list[B] = []
    for x in xs:
        y = f(x)
        if y is not None:
            result.append(y)
    return tuple(result)


def maybe[A, B](default: B, f: Callable[[A], B], x: A | None) -> B:
    """Handle an optional value, with transformation."""
    return default if x is None else f(x)


def pure[A](x: A) -> A | None:
    """Lift a value into the Maybe type."""
    return x

