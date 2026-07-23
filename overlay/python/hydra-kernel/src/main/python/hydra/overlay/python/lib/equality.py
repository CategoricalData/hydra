"""Python implementations of hydra.lib.equality primitives."""

from __future__ import annotations
from typing import Any


def equal(a: Any, b: Any) -> bool:
    """Check if two values are equal."""
    return a == b
