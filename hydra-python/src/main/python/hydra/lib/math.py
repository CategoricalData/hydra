"""Python implementations of hydra.lib.math primitives."""

from hydra.dsl.python import frozenlist


def abs_(x: int) -> int:
    """Return the absolute value of an integer."""
    return abs(x)


def add(x: int, y: int) -> int:
    """Add two integers."""
    return x + y


def div(x: int, y: int) -> int:
    """Divide two integers using integer division."""
    return x // y


def even(x: int) -> bool:
    """Check if an integer is even."""
    return x % 2 == 0


def mod(a: int, b: int) -> int:
    """Mathematical modulo with result having same sign as divisor."""
    return a % b


def mul(x: int, y: int) -> int:
    """Multiply two integers."""
    return x * y


def negate(x: int) -> int:
    """Negate an integer."""
    return -x


def odd(x: int) -> bool:
    """Check if an integer is odd."""
    return x % 2 != 0


def pred(x: int) -> int:
    """Return the predecessor of an integer (x - 1)."""
    return x - 1


def range_(start: int, end: int) -> frozenlist[int]:
    """Generate a range of integers from start to end (inclusive)."""
    return tuple(range(start, end + 1))


def rem(a: int, b: int) -> int:
    """Integer remainder with result having same sign as dividend."""
    return a - (a // b) * b


def signum(x: int) -> int:
    """Return the sign of an integer (-1, 0, or 1)."""
    if x < 0:
        return -1
    elif x > 0:
        return 1
    else:
        return 0


def sub(x: int, y: int) -> int:
    """Subtract two integers."""
    return x - y


def succ(x: int) -> int:
    """Return the successor of an integer (x + 1)."""
    return x + 1
