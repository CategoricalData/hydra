"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from decimal import Decimal
from typing import Any

from hydra.overlay.python.util._decimal import decimal_key


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal."""
    # Decimals: equal iff both value and scale agree (docs/specification/
    # ordering-and-equality.md); Decimal's native == ignores scale.
    if isinstance(a, Decimal) and isinstance(b, Decimal):
        return decimal_key(a) == decimal_key(b)
    return a == b
