"""Python implementations of hydra.lib.math primitives."""

from hydra.dsl.python import frozenlist


def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y


def div(x: int, y: int) -> int:
    """Divide two integers using integer division."""
    return x // y


def mod(a: int, b: int) -> int:
    """Mathematical modulo with result having same sign as divisor."""
    return a % b


def mul(x: int, y: int) -> int:
    """Multiply two integers."""
    return x * y


def negate(x: int) -> int:
    """Negate an integer."""
    return -x


def range_(start: int, end: int) -> frozenlist[int]:
    """Generate a range of integers from start to end (inclusive)."""
    return tuple(range_(start, end + 1))


def rem(a: int, b: int) -> int:
    """Integer remainder with result having same sign as dividend."""
    return a - (a // b) * b


def sub(x: int, y: int) -> int:
    """Subtract two integers."""
    return x - y
