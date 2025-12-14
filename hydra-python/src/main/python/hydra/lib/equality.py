"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from typing import Any, TypeVar
import hydra.util

A = TypeVar('A')


def compare(x: Any, y: Any) -> hydra.util.Comparison:
    """Compare two values and return a Comparison."""
    if x < y:
        return hydra.util.Comparison.LESS_THAN
    elif x > y:
        return hydra.util.Comparison.GREATER_THAN
    else:
        return hydra.util.Comparison.EQUAL_TO


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal."""
    return a == b


def gt(a: Any, b: Any) -> bool:
    """Check if first value is greater than second."""
    return a > b


def gte(a: Any, b: Any) -> bool:
    """Check if first value is greater than or equal to second."""
    return a >= b


def identity(a: A) -> A:
    """Return the identity of a value."""
    return a


def lt(a: Any, b: Any) -> bool:
    """Check if first value is less than second."""
    return a < b


def lte(a: Any, b: Any) -> bool:
    """Check if first value is less than or equal to second."""
    return a <= b


def max(a: Any, b: Any) -> Any:
    """Return the maximum of two values."""
    return a if a > b else b


def min(a: Any, b: Any) -> Any:
    """Return the minimum of two values."""
    return a if a < b else b
