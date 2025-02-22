"""Python implementations of hydra.lib.strings primitives."""


def cat(xs: list[str]) -> str:
    """Concatenate a list of strings."""
    return "".join(xs)


def cat2(s1: str, s2: str) -> str:
    """Concatenate two strings."""
    return s1 + s2


def from_list(values: list[int]) -> str:
    """Convert a list of integers to a string."""
    return "".join(chr(v) for v in values)


def intercalate(separator: str, values: list[str]) -> str:
    """Intercalate a string between a list of strings."""
    return separator.join(values)


def is_empty(x: str) -> bool:
    """Check if a string is empty."""
    return len(x) == 0


def length(x: str) -> int:
    """Get the length of a string."""
    return len(x)


def split_on(delimiter: str, x: str) -> list[str]:
    """Split a string on a delimiter."""
    return x.split(delimiter)


def to_list(x: str) -> list[int]:
    """Convert a string to a list of integers."""
    return [ord(c) for c in x]


def to_lower(x: str) -> str:
    """Convert a string to lowercase."""
    return x.lower()


def to_upper(x: str) -> str:
    """Convert a string to uppercase."""
    return x.upper()
