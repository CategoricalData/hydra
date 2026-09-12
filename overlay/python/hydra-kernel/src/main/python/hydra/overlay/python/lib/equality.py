"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from typing import Any

from hydra.overlay.python.util._compare import compare as _compare_terms


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal.

    Delegates entirely to the shared structural comparator (hydra.overlay.python.util
    ._compare, also used by hydra.lib.ordering.compare) rather than native ``==``:
    the ``a -> a -> Bool`` primitive signature is polymorphic, so ``a``/``b`` here are
    often raw (undecoded) ``Term`` values, not native Python values -- native ``==``
    on a ``Term`` recurses via dataclass field equality and reaches native ``==`` on
    any Decimal/float leaf, which is scale-blind for Decimal and not Hydra's extended
    totalOrder for float (NaN != NaN; -0.0 == 0.0), silently wrong per docs/
    specification/ordering-and-equality.md. ``_compare_terms`` already handles both
    cases correctly at any nesting depth (map/set/record values included).
    """
    return _compare_terms(a, b) == 0
