"""Python implementations of hydra.lib.logic (logic and control flow) primitives."""

from typing import Callable, overload


def and_(x: bool, y: bool) -> bool:
    """Compute the logical AND of two boolean values."""
    return x and y


@overload
def if_else[A](b: bool, x: Callable[[], A], y: Callable[[], A]) -> A: ...
@overload
def if_else[A](b: bool, x: A, y: A) -> A: ...
def if_else[A](b: bool, x: A | Callable[[], A], y: A | Callable[[], A]) -> A:
    """Compute a conditional expression with lazy evaluation of branches.

    When x and y are callable (lambdas), only the chosen branch is called.
    This enables lazy evaluation for expensive computations or side-effecting operations,
    which is critical for correct Flow/state monad semantics.
    """
    if b:
        return x() if callable(x) else x  # type: ignore[return-value]
    else:
        return y() if callable(y) else y  # type: ignore[return-value]


def not_(x: bool) -> bool:
    """Compute the logical NOT of a boolean value."""
    return not x


def or_(x: bool, y: bool) -> bool:
    """Compute the logical OR of two boolean values."""
    return x or y
