"""Python implementations of hydra.core.lib.chars primitives."""

from __future__ import annotations

# Python's str.lower()/upper() implement Unicode's FULL case mapping, which
# can differ from the SIMPLE (one-to-one) mapping this module needs. Falling
# back to "unchanged" whenever the full mapping isn't a single code point is
# correct for every divergent code point except the ones below, where a
# length-1 simple mapping exists but the full mapping is longer. Found by
# diffing Python's str.lower()/upper() against Java's Character.toLowerCase/
# toUpperCase (which expose Unicode's simple mapping directly) across every
# code point that changes case.
_SIMPLE_LOWER_OVERRIDES = {
    0x0130: 0x0069,  # İ LATIN CAPITAL LETTER I WITH DOT ABOVE -> i
}
_SIMPLE_UPPER_OVERRIDES = {
    # Greek letters with iota subscript: simple uppercase mapping adds the
    # iota as a capital letter (iota subscript -> capital iota), while the
    # full mapping spells out the base letter's own uppercase form followed
    # by a capital iota (2 code points).
    0x1F80: 0x1F88, 0x1F81: 0x1F89, 0x1F82: 0x1F8A, 0x1F83: 0x1F8B,
    0x1F84: 0x1F8C, 0x1F85: 0x1F8D, 0x1F86: 0x1F8E, 0x1F87: 0x1F8F,
    0x1F90: 0x1F98, 0x1F91: 0x1F99, 0x1F92: 0x1F9A, 0x1F93: 0x1F9B,
    0x1F94: 0x1F9C, 0x1F95: 0x1F9D, 0x1F96: 0x1F9E, 0x1F97: 0x1F9F,
    0x1FA0: 0x1FA8, 0x1FA1: 0x1FA9, 0x1FA2: 0x1FAA, 0x1FA3: 0x1FAB,
    0x1FA4: 0x1FAC, 0x1FA5: 0x1FAD, 0x1FA6: 0x1FAE, 0x1FA7: 0x1FAF,
    0x1FB3: 0x1FBC, 0x1FC3: 0x1FCC, 0x1FF3: 0x1FFC,
}


def is_alpha_num(value: int) -> bool:
    """Check whether a character is alphanumeric."""
    return chr(value).isalnum()


def is_lower(value: int) -> bool:
    """Check whether a character is lowercase."""
    return chr(value).islower()


def is_space(value: int) -> bool:
    """Check whether a character is a whitespace character."""
    return chr(value).isspace()


def is_upper(value: int) -> bool:
    """Check whether a character is uppercase."""
    return chr(value).isupper()


def to_lower(value: int) -> int:
    """Convert a character to lowercase."""
    if value in _SIMPLE_LOWER_OVERRIDES:
        return _SIMPLE_LOWER_OVERRIDES[value]
    # str.lower() performs full Unicode case folding, which can turn one
    # code point into several (e.g. U+00DF "ß" would stay "ß" since it has
    # no simple lowercase mapping, but other code points genuinely expand).
    # Python has no simple (one-to-one) case-mapping API, so fall back to
    # the input unchanged when that happens, per the simple-mapping spec.
    lowered = chr(value).lower()
    return ord(lowered) if len(lowered) == 1 else value


def to_upper(value: int) -> int:
    """Convert a character to uppercase."""
    if value in _SIMPLE_UPPER_OVERRIDES:
        return _SIMPLE_UPPER_OVERRIDES[value]
    uppered = chr(value).upper()
    return ord(uppered) if len(uppered) == 1 else value
