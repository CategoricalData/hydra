"""Python implementations of hydra.lib.chars primitives."""


def is_alpha_num(c: int) -> bool:
    """Check if a character is alphanumeric."""
    return chr(c).isalnum()


def is_lower(c: int) -> bool:
    """Check if a character is lowercase."""
    return chr(c).islower()


def is_space(c: int) -> bool:
    """Check if a character is a whitespace character."""
    return chr(c).isspace()


def is_upper(c: int) -> bool:
    """Check if a character is uppercase."""
    return chr(c).isupper()


def to_lower(c: int) -> int:
    """Convert a character to lowercase."""
    return ord(chr(c).lower())


def to_upper(c: int) -> int:
    """Convert a character to uppercase."""
    return ord(chr(c).upper())
