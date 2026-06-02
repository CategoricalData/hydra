"""Phantom-typed term DSL for the hydra.lib.pairs library."""

from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1, lib_primitive3 as primitive3


def bimap(f: TypedTerm, g: TypedTerm, p: TypedTerm) -> TypedTerm:
    """Map over both elements of a pair."""
    return primitive3(f, g, p)


def first(pair: TypedTerm) -> TypedTerm:
    """
    Get the first element of a pair.

    Type: (a, b) -> a
    """
    return primitive1(pair)


def second(pair: TypedTerm) -> TypedTerm:
    """
    Get the second element of a pair.

    Type: (a, b) -> b
    """
    return primitive1(pair)
