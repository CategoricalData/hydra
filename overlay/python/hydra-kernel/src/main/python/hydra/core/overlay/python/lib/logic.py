"""Python implementations of hydra.core.lib.logic (logic and control flow) primitives."""

from __future__ import annotations
from typing import Callable, overload, TypeVar

A = TypeVar('A')


def and_(x: bool, y: bool | Callable[[], bool]) -> bool:
    """Compute the logical AND of two boolean values. The second operand is lazy (#391, #802)."""
    return bool(x and (y() if callable(y) else y))


@overload
def if_else(b: bool, x: Callable[[], A], y: Callable[[], A]) -> A: ...
@overload
def if_else(b: bool, x: A, y: A) -> A: ...
def if_else(b: bool, x: A | Callable[[], A], y: A | Callable[[], A]) -> A:
    """Compute a conditional expression."""
    if b:
        return x() if callable(x) else x  # type: ignore[return-value]
    else:
        return y() if callable(y) else y  # type: ignore[return-value]


def not_(x: bool) -> bool:
    """Compute the logical NOT of a boolean value."""
    return not x


def or_(x: bool, y: bool | Callable[[], bool]) -> bool:
    """Compute the logical OR of two boolean values. The second operand is lazy (#391, #802)."""
    return bool(x or (y() if callable(y) else y))
