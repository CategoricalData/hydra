"""Python implementations of hydra.lib.optionals primitives."""

from collections.abc import Callable
from typing import Any


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


def cat[A](xs: list[A | None]) -> list[A]:
    """Filter out None values from a list."""
    return [x for x in xs if x is not None]


def compose[A, B, C](
    f: Callable[[A], B | None], g: Callable[[B], C | None]
) -> Callable[[A], C | None]:
    """Compose two functions."""
    return lambda x: bind(f(x), g)


def from_maybe[A](x: A, y: A | None) -> A:
    """Get a value from an optional value, or return a default value."""
    return y if y is not None else x


def is_just(x: Any | None) -> bool:
    """Check if a value is not None."""
    return x is not None


def is_nothing(x: Any | None) -> bool:
    """Check if a value is None."""
    return x is None


def map[A, B](f: Callable[[A], B], x: A | None) -> B | None:
    """Map a function over an optional value."""
    return f(x) if x is not None else None


def maybe[A, B](x: B, f: Callable[[A], B], y: A | None) -> B:
    """Handle an optional value, with transformation."""
    return x if y is None else f(y)


def pure[A](x: A) -> A | None:
    """Lift a value into the Maybe type."""
    return x
