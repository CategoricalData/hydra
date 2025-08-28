"""Python implementations of hydra.lib.chars primitives."""


def is_alpha_num(value: int) -> bool:
    """Check if a character (as int) is alphanumeric."""
    return chr(value).isalnum()


def is_lower(value: int) -> bool:
    """Check if a character (as int) is lowercase."""
    return chr(value).islower()


def is_space(value: int) -> bool:
    """Check if a character (as int) is whitespace."""
    return chr(value).isspace()


def is_upper(value: int) -> bool:
    """Check if a character (as int) is uppercase."""
    return chr(value).isupper()


def to_lower(value: int) -> int:
    """Convert a character (as int) to lowercase, returning the int value."""
    return ord(chr(value).lower())


def to_upper(value: int) -> int:
    """Convert a character (as int) to uppercase, returning the int value."""
    return ord(chr(value).upper())
