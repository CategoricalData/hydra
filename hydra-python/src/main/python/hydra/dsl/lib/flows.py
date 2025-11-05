"""Phantom-typed term DSL for the hydra.lib.flows library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive as primitive, lib_primitive1 as primitive1, lib_primitive2 as primitive2, lib_primitive3 as primitive3


def apply(f: TTerm, x: TTerm) -> TTerm:
    """Apply a function in a Flow to a value in a Flow (applicative)."""
    return primitive2(f, x)


def bind(mx: TTerm, f: TTerm) -> TTerm:
    """Monadic bind for Flow."""
    return primitive2(mx, f)


def fail(message: TTerm) -> TTerm:
    """Create a failed Flow with an error message."""
    return primitive1(message)


def foldl(f: TTerm, initial: TTerm, values: TTerm) -> TTerm:
    """Fold over a list with a monadic function."""
    return primitive3(f, initial, values)


def map(f: TTerm, mx: TTerm) -> TTerm:
    """Map a function over a Flow (functor)."""
    return primitive2(f, mx)


def map_elems(f: TTerm, m: TTerm) -> TTerm:
    """Map a monadic function over the values of a map."""
    return primitive2(f, m)


def map_keys(f: TTerm, m: TTerm) -> TTerm:
    """Map a monadic function over the keys of a map."""
    return primitive2(f, m)


def map_list(f: TTerm, xs: TTerm) -> TTerm:
    """Map a monadic function over a list."""
    return primitive2(f, xs)


def map_maybe(f: TTerm, mx: TTerm) -> TTerm:
    """Map a monadic function over an optional value."""
    return primitive2(f, mx)


def map_set(f: TTerm, xs: TTerm) -> TTerm:
    """Map a monadic function over a set."""
    return primitive2(f, xs)


def pure(x: TTerm) -> TTerm:
    """Lift a value into a Flow."""
    return primitive1(x)


def sequence(flows: TTerm) -> TTerm:
    """Sequence a list of Flows into a Flow of a list."""
    return primitive1(flows)
