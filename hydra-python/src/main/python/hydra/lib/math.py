"""Python implementations of hydra.lib.math primitives."""


def neg(x: int) -> int:
    """Negate an integer."""
    return -x


def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y


def sub(x: int, y: int) -> int:
    """Subtract two integers."""
    return x - y


def mul(x: int, y: int) -> int:
    """Multiply two integers."""
    return x * y


def div(x: int, y: int) -> int:
    """Divide two integers."""
    return x // y


def mod(a: int, b: int) -> int:
    """Mathematical modulo with result having same sign as divisor."""
    return a % b


def rem(a: int, b: int) -> int:
    """Integer remainder with result having same sign as dividend."""
    return a - (b * (a // b))
