"""Python implementations of hydra.lib.functions primitives."""

from __future__ import annotations
from typing import TypeVar

A = TypeVar('A')


def identity(a: A) -> A:
    """Return a value unchanged."""
    return a
