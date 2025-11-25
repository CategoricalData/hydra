"""Python implementations of hydra.lib.logic (logic and control flow) primitives."""


def and_(x: bool, y: bool) -> bool:
    """Compute the logical AND of two boolean values."""
    return x and y


def if_else[A](b: bool, x: A, y: A) -> A:
    """Compute a conditional expression with lazy evaluation of branches.

    When x and y are callable (lambdas), they are called to get the actual value.
    This enables lazy evaluation for expensive computations or side-effecting operations.
    """
    # If the arguments are callable (thunks), call them to get the actual values
    actual_x = x() if callable(x) else x
    actual_y = y() if callable(y) else y
    return actual_x if b else actual_y


def not_(x: bool) -> bool:
    """Compute the logical NOT of a boolean value."""
    return not x


def or_(x: bool, y: bool) -> bool:
    """Compute the logical OR of two boolean values."""
    return x or y
