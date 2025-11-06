"""Phantom-typed term DSL for the hydra.lib.pairs library."""

from hydra.phantoms import TTerm
from hydra.dsl.phantoms import lib_primitive1 as primitive1


def first(pair: TTerm) -> TTerm:
    """
    Get the first element of a pair.

    Type: (a, b) -> a
    """
    return primitive1(pair)


def second(pair: TTerm) -> TTerm:
    """
    Get the second element of a pair.

    Type: (a, b) -> b
    """
    return primitive1(pair)
