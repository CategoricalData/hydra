"""Phantom-typed term DSL for the hydra.lib.maybes library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def apply(f: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Apply a function to an argument."""
    return primitive2(f, x)


def bind(x: TypedTerm, f: TypedTerm) -> TypedTerm:
    """Chain operations on optional values, handling Nothing cases automatically."""
    return primitive2(x, f)


def cases(m: TypedTerm, n: TypedTerm, j: TypedTerm) -> TypedTerm:
    """Handle an optional value with different parameter order than maybe."""
    return primitive3(m, n, j)


def cat(xs: TypedTerm) -> TypedTerm:
    """Filter out Nothing values from a list."""
    return primitive1(xs)


def compose(f: TypedTerm, g: TypedTerm) -> TypedTerm:
    """Compose two functions."""
    return primitive2(f, g)


def from_just(x: TypedTerm) -> TypedTerm:
    """Extract value from Maybe, assuming it's Just (unsafe)."""
    return primitive1(x)


def from_maybe(default: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Get a value from an optional value, or return a default value."""
    return primitive2(default, x)


def is_just(x: TypedTerm) -> TypedTerm:
    """Check if a value is Just."""
    return primitive1(x)


def is_nothing(x: TypedTerm) -> TypedTerm:
    """Check if a value is Nothing."""
    return primitive1(x)


def map(f: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Map a function over an optional value."""
    return primitive2(f, x)


def map_maybe(f: TypedTerm, xs: TypedTerm) -> TypedTerm:
    """Map a function over a list and collect Just results."""
    return primitive2(f, xs)


def maybe(default: TypedTerm, f: TypedTerm, x: TypedTerm) -> TypedTerm:
    """Handle an optional value, with transformation."""
    return primitive3(default, f, x)


def pure(x: TypedTerm) -> TypedTerm:
    """Lift a value into the Maybe type."""
    return primitive1(x)


def to_list(x: TypedTerm) -> TypedTerm:
    """Convert a Maybe to a list: Just x becomes [x], Nothing becomes []."""
    return primitive1(x)
