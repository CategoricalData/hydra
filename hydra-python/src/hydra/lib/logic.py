"""Python implementations of hydra.lib.logic (logic and control flow) primitives."""


def and_(x: bool, y: bool) -> bool:
    """Compute the logical AND of two boolean values."""
    return x and y


def if_else[A](b: bool, x: A, y: A) -> A:
    """Compute a conditional expression."""
    return x if b else y


def not_(x: bool) -> bool:
    """Compute the logical NOT of a boolean value."""
    return not x


def or_(x: bool, y: bool) -> bool:
    """Compute the logical OR of two boolean values."""
    return x or y
