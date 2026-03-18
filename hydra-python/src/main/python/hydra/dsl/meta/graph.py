"""Meta-DSL for constructing graph-related terms (Graph, Primitive, Comparison, TypeClass).

Mirrors the Haskell module Hydra.Dsl.Meta.Graph, providing phantom-typed constructors,
accessors, and modifiers for graph-related Hydra types.

Most functions are re-exported from the generated module hydra.dsl.graph. This module
adds custom helpers for Comparison and TypeClass union constructors.
"""

import hydra.dsl.meta.phantoms as Phantoms
from hydra.classes import TypeClass
from hydra.core import Name
from hydra.phantoms import TTerm

# Re-export everything from the generated DSL module.
from hydra.dsl.graph import *  # noqa: F401, F403


# ============================================================
# Comparison (custom -- not in hydra.graph type module)
# ============================================================

def comparison_less_than() -> TTerm:
    """The lessThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_LESS_THAN = Name("lessThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_LESS_THAN)


def comparison_equal_to() -> TTerm:
    """The equalTo Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_EQUAL_TO = Name("equalTo")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_EQUAL_TO)


def comparison_greater_than() -> TTerm:
    """The greaterThan Comparison variant."""
    _COMPARISON_NAME = Name("hydra.util.Comparison")
    _COMPARISON_GREATER_THAN = Name("greaterThan")
    return Phantoms.inject_unit(_COMPARISON_NAME, _COMPARISON_GREATER_THAN)


# ============================================================
# TypeClass (custom -- not in hydra.graph type module)
# ============================================================

def type_class_equality() -> TTerm:
    """The equality TypeClass variant."""
    return Phantoms.inject_unit(TypeClass.TYPE_, TypeClass.EQUALITY.value)


def type_class_ordering() -> TTerm:
    """The ordering TypeClass variant."""
    return Phantoms.inject_unit(TypeClass.TYPE_, TypeClass.ORDERING.value)
