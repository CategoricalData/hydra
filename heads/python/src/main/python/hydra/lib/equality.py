"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from typing import Any, TypeVar
import hydra.util

A = TypeVar('A')


def compare(x: Any, y: Any) -> hydra.util.Comparison:
    """Compare two values and return a Comparison.

    Native Python values (ints, strings, etc.) use Python's built-in
    comparison. For Hydra Term values, the auto-generated `__lt__` walks
    into nested record/dict-typed fields that lack `<`, so we catch the
    resulting TypeError and fall back to structural comparison mirroring
    the Java implementation in hydra.lib.equality.Compare: same-variant
    literals are compared by value, otherwise we fall back to the show
    representation.
    """
    try:
        if x < y:
            cmp = -1
        elif x > y:
            cmp = 1
        else:
            cmp = 0
    except TypeError:
        cmp = _compare_terms(x, y)
    if cmp < 0:
        return hydra.util.Comparison.LESS_THAN
    elif cmp > 0:
        return hydra.util.Comparison.GREATER_THAN
    else:
        return hydra.util.Comparison.EQUAL_TO


def _compare_terms(t1: Any, t2: Any) -> int:
    import hydra.core
    if isinstance(t1, hydra.core.TermLiteral) and isinstance(t2, hydra.core.TermLiteral):
        return _compare_literals(t1.value, t2.value)
    import hydra.show.core
    s1 = hydra.show.core.term(t1)
    s2 = hydra.show.core.term(t2)
    return (s1 > s2) - (s1 < s2)


def _compare_literals(l1: Any, l2: Any) -> int:
    import hydra.core
    # Same-variant: compare wrapped values directly (Python ints, strs, bools,
    # floats, and Decimals all support < natively).
    if isinstance(l1, hydra.core.LiteralInteger) and isinstance(l2, hydra.core.LiteralInteger):
        v1 = l1.value.value
        v2 = l2.value.value
        return (v1 > v2) - (v1 < v2)
    if isinstance(l1, hydra.core.LiteralFloat) and isinstance(l2, hydra.core.LiteralFloat):
        v1 = l1.value.value
        v2 = l2.value.value
        return (v1 > v2) - (v1 < v2)
    if isinstance(l1, hydra.core.LiteralString) and isinstance(l2, hydra.core.LiteralString):
        v1 = l1.value
        v2 = l2.value
        return (v1 > v2) - (v1 < v2)
    if isinstance(l1, hydra.core.LiteralBoolean) and isinstance(l2, hydra.core.LiteralBoolean):
        v1 = l1.value
        v2 = l2.value
        return (v1 > v2) - (v1 < v2)
    if isinstance(l1, hydra.core.LiteralDecimal) and isinstance(l2, hydra.core.LiteralDecimal):
        v1 = l1.value
        v2 = l2.value
        return (v1 > v2) - (v1 < v2)
    # Different variants: fall back to show representation
    import hydra.show.core
    s1 = hydra.show.core.literal(l1)
    s2 = hydra.show.core.literal(l2)
    return (s1 > s2) - (s1 < s2)


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
    """Return a value unchanged."""
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
