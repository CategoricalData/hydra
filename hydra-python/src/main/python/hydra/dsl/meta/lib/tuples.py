"""Phantom-typed term DSL for the hydra.lib.tuples library."""

from hydra.phantoms import TTerm
from hydra.dsl.meta.phantoms import lib_primitive1 as primitive1


def curry(f: TTerm) -> TTerm:
    """
    Convert a function that takes a pair to a curried function.

    Type: ((a, b) -> c) -> (a -> b -> c)
    """
    return primitive1(f)


def fst(pair: TTerm) -> TTerm:
    """
    Get the first element of a pair.

    Type: (a, b) -> a
    """
    return primitive1(pair)


def snd(pair: TTerm) -> TTerm:
    """
    Get the second element of a pair.

    Type: (a, b) -> b
    """
    return primitive1(pair)


def uncurry(f: TTerm) -> TTerm:
    """
    Convert a curried function to a function that takes a pair.

    Type: (a -> b -> c) -> ((a, b) -> c)
    """
    return primitive1(f)
