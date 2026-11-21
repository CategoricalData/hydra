"""Python implementations of hydra.core.lib.equality primitives."""

from __future__ import annotations
from typing import Any

from hydra.core.overlay.python.util._compare import compare as _compare_terms

# Leaf types whose native ``==`` agrees with the structural comparator (#850).
_NATIVE_EQ_TYPES = (str, int, bool, bytes)


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal.

    Delegates entirely to the shared structural comparator (hydra.core.overlay.python.util
    ._compare, also used by hydra.core.lib.ordering.compare) rather than native ``==``:
    the ``a -> a -> Bool`` primitive signature is polymorphic, so ``a``/``b`` here are
    often raw (undecoded) ``Term`` values, not native Python values -- native ``==``
    on a ``Term`` recurses via dataclass field equality and reaches native ``==`` on
    any Decimal/float leaf, which is scale-blind for Decimal and not Hydra's extended
    totalOrder for float (NaN != NaN; -0.0 == 0.0), silently wrong per docs/
    specification/ordering-and-equality.md. ``_compare_terms`` already handles both
    cases correctly at any nesting depth (map/set/record values included).

    Fast path (#850): when both arguments have the same exact type and it is one of
    ``_NATIVE_EQ_TYPES``, native ``==`` gives the comparator's answer without its dispatch.
    Exact-type matching keeps ``True`` vs ``1`` and str subclasses on the structural path.
    """
    t = type(a)
    if t is type(b) and t in _NATIVE_EQ_TYPES:
        return a == b
    return _compare_terms(a, b) == 0
