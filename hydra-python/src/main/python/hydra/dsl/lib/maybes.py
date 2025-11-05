"""Phantom-typed term DSL for the hydra.lib.maybes library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def apply(f: TTerm, x: TTerm) -> TTerm:
    """Apply a function to an argument."""
    return primitive2(f, x)


def bind(x: TTerm, f: TTerm) -> TTerm:
    """Chain operations on optional values, handling Nothing cases automatically."""
    return primitive2(x, f)


def cases(m: TTerm, n: TTerm, j: TTerm) -> TTerm:
    """Handle an optional value with different parameter order than maybe."""
    return primitive3(m, n, j)


def cat(xs: TTerm) -> TTerm:
    """Filter out Nothing values from a list."""
    return primitive1(xs)


def compose(f: TTerm, g: TTerm) -> TTerm:
    """Compose two functions."""
    return primitive2(f, g)


def from_just(x: TTerm) -> TTerm:
    """Extract value from Maybe, assuming it's Just (unsafe)."""
    return primitive1(x)


def from_maybe(default: TTerm, x: TTerm) -> TTerm:
    """Get a value from an optional value, or return a default value."""
    return primitive2(default, x)


def is_just(x: TTerm) -> TTerm:
    """Check if a value is Just."""
    return primitive1(x)


def is_nothing(x: TTerm) -> TTerm:
    """Check if a value is Nothing."""
    return primitive1(x)


def map(f: TTerm, x: TTerm) -> TTerm:
    """Map a function over an optional value."""
    return primitive2(f, x)


def map_maybe(f: TTerm, xs: TTerm) -> TTerm:
    """Map a function over a list and collect Just results."""
    return primitive2(f, xs)


def maybe(default: TTerm, f: TTerm, x: TTerm) -> TTerm:
    """Handle an optional value, with transformation."""
    return primitive3(default, f, x)


def pure(x: TTerm) -> TTerm:
    """Lift a value into the Maybe type."""
    return primitive1(x)
