"""Python implementations of hydra.lib.ordering primitives."""

from __future__ import annotations
from typing import Any
import hydra.util
from hydra.overlay.python.util._compare import compare as _compare_terms


def compare(x: Any, y: Any) -> hydra.util.Comparison:
    """Compare two values structurally and return a Comparison.

    Delegates to ``hydra.overlay.python.util._compare.compare``, the shared
    structural comparator (also used to order ``PersistentMap``/``PersistentSet``
    keys), so that the ``hydra.lib.ordering.compare`` primitive matches
    docs/specification/ordering-and-equality.md exactly: no print-based
    fallback anywhere.
    """
    cmp = _compare_terms(x, y)
    if cmp < 0:
        return hydra.util.Comparison.LESS_THAN
    elif cmp > 0:
        return hydra.util.Comparison.GREATER_THAN
    else:
        return hydra.util.Comparison.EQUAL_TO


def gt(a: Any, b: Any) -> bool:
    """Check if first value is greater than second."""
    return _compare_terms(a, b) > 0


def gte(a: Any, b: Any) -> bool:
    """Check if first value is greater than or equal to second."""
    return _compare_terms(a, b) >= 0


def lt(a: Any, b: Any) -> bool:
    """Check if first value is less than second."""
    return _compare_terms(a, b) < 0


def lte(a: Any, b: Any) -> bool:
    """Check if first value is less than or equal to second."""
    return _compare_terms(a, b) <= 0


def max(a: Any, b: Any) -> Any:
    """Return the maximum of two values."""
    return a if _compare_terms(a, b) >= 0 else b


def min(a: Any, b: Any) -> Any:
    """Return the minimum of two values."""
    return a if _compare_terms(a, b) <= 0 else b
