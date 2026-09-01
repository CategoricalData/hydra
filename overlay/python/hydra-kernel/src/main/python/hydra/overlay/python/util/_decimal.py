"""Scale-aware equality and hashing for Decimal-wrapping nodes.

Implements docs/specification/ordering-and-equality.md: two decimals are equal
exactly when both their numeric value and their scale agree (``1.1`` and
``1.10`` are distinct). Python's native ``Decimal.__eq__``/``__hash__`` ignore
scale, so ``LiteralDecimal`` (a ``Node[Decimal]``) overrides them here rather
than relying on ``Node``'s generic, scale-blind derivation.

Ordering is handled separately by ``hydra.overlay.python.util._compare``,
which already implements the value-then-scale tiebreak.
"""

from __future__ import annotations

from decimal import Decimal


def decimal_key(value: Decimal) -> tuple:
    """A hashable key that distinguishes decimals by scale as well as value."""
    return (value, -value.as_tuple().exponent)


def decimal_node_eq(self, other: object) -> bool:
    if type(other) is not type(self):
        return NotImplemented
    return decimal_key(self.value) == decimal_key(other.value)


def decimal_node_hash(self) -> int:
    return hash(decimal_key(self.value))
