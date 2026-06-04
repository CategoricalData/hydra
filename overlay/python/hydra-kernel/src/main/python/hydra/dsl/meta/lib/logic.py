"""Phantom-typed term DSL for the hydra.lib.logic library."""

from hydra.core import Name
from hydra.typed import TypedTerm
from hydra.dsl.meta.phantoms import (
    apply,
    fold,
    lib_primitive1 as primitive1,
    lib_primitive2 as primitive2,
    lib_primitive3 as primitive3,
    primitive,
)
from hydra.dsl.meta.phantom_literals import true, false


def and_(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Compute the logical AND of two boolean values."""
    return primitive2(x, y)


def ands(terms: TypedTerm) -> TypedTerm:
    """Fold a list of booleans with logical AND, returning True for an empty list."""
    return apply(apply(fold(primitive(Name("hydra.lib.logic.and"))), true()), terms)


def if_else(b: TypedTerm, x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Compute a conditional expression."""
    return primitive3(b, x, y)


def not_(x: TypedTerm) -> TypedTerm:
    """Compute the logical NOT of a boolean value."""
    return primitive1(x)


def or_(x: TypedTerm, y: TypedTerm) -> TypedTerm:
    """Compute the logical OR of two boolean values."""
    return primitive2(x, y)


def ors(terms: TypedTerm) -> TypedTerm:
    """Fold a list of booleans with logical OR, returning False for an empty list."""
    return apply(apply(fold(primitive(Name("hydra.lib.logic.or"))), false()), terms)
