"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from decimal import Decimal
from typing import Any

from hydra.overlay.python.util._compare import compare as _compare_terms
from hydra.overlay.python.util._decimal import decimal_key


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal.

    Decimals and floats each need special-case handling beyond native ``==``, since
    Python's built-in comparison is scale-blind for ``Decimal`` and IEEE-754-native
    (rather than Hydra's extended totalOrder) for ``float``:

    - Decimals: equal iff both value and scale agree (docs/specification/
      ordering-and-equality.md); Decimal's native == ignores scale.
    - Floats: route through the shared structural comparator so that ``equal`` agrees
      with ``hydra.lib.ordering.compare`` on the IEEE 754 extended totalOrder: NaN equal
      to itself, -0.0 unequal to +0.0 -- native Python ``==`` gets both wrong (NaN !=
      NaN; -0.0 == 0.0).
    """
    if isinstance(a, Decimal) and isinstance(b, Decimal):
        return decimal_key(a) == decimal_key(b)
    if isinstance(a, float) and isinstance(b, float):
        return _compare_terms(a, b) == 0
    return a == b
