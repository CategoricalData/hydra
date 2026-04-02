# Note: this is an automatically generated file. Do not edit.

r"""General-purpose utility types used across Hydra."""

from __future__ import annotations
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Node
from typing import TypeAlias, cast
import hydra.core

class CaseConvention(Enum):
    r"""A naming convention for symbols, such as camelCase or snake_case."""

    CAMEL = hydra.core.Name("camel")

    PASCAL = hydra.core.Name("pascal")

    LOWER_SNAKE = hydra.core.Name("lowerSnake")

    UPPER_SNAKE = hydra.core.Name("upperSnake")

CaseConvention.TYPE_ = hydra.core.Name("hydra.util.CaseConvention")

class Comparison(Enum):
    r"""An equality judgement: less than, equal to, or greater than."""

    LESS_THAN = hydra.core.Name("lessThan")

    EQUAL_TO = hydra.core.Name("equalTo")

    GREATER_THAN = hydra.core.Name("greaterThan")

Comparison.TYPE_ = hydra.core.Name("hydra.util.Comparison")

class PrecisionArbitrary:
    r"""Arbitrary precision"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrecisionArbitrary)
    def __hash__(self):
        return hash("PrecisionArbitrary")

class PrecisionBits(Node[int]):
    r"""Precision to a specified number of bits"""

class _PrecisionMeta(type):
    def __getitem__(cls, item):
        return object

# Numeric precision: arbitrary precision, or precision to a specified number of bits.
class Precision(metaclass=_PrecisionMeta):
    r"""PrecisionArbitrary | PrecisionBits"""

    TYPE_ = hydra.core.Name("hydra.util.Precision")
    ARBITRARY = hydra.core.Name("arbitrary")
    BITS = hydra.core.Name("bits")
