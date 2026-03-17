# Note: this is an automatically generated file. Do not edit.

r"""General-purpose utility types used across Hydra."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Either, Node
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.context
import hydra.core
import hydra.error

T1 = TypeVar("T1")
T2 = TypeVar("T2")
V1 = TypeVar("V1")
V2 = TypeVar("V2")

@dataclass(frozen=True)
class Adapter(Generic[T1, T2, V1, V2]):
    r"""A two-level bidirectional encoder which adapts types to types and terms to terms."""

    is_lossy: Annotated[bool, "Whether information may be lost in the course of this adaptation"]
    source: Annotated[T1, "The source type"]
    target: Annotated[T2, "The target type"]
    coder: Annotated[Coder[V1, V2], "The coder for transforming instances of the source type to instances of the target type"]

    TYPE_ = hydra.core.Name("hydra.util.Adapter")
    IS_LOSSY = hydra.core.Name("isLossy")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")
    CODER = hydra.core.Name("coder")

@dataclass(frozen=True)
class Bicoder(Generic[T1, T2, V1, V2]):
    r"""A two-level encoder and decoder, operating both at a type level and an instance (data) level."""

    encode: Annotated[Callable[[T1], Adapter[T1, T2, V1, V2]], "A function from source types to adapters"]
    decode: Annotated[Callable[[T2], Adapter[T2, T1, V2, V1]], "A function from target types to adapters"]

    TYPE_ = hydra.core.Name("hydra.util.Bicoder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

class CaseConvention(Enum):
    r"""A naming convention for symbols, such as camelCase or snake_case."""

    CAMEL = hydra.core.Name("camel")

    PASCAL = hydra.core.Name("pascal")

    LOWER_SNAKE = hydra.core.Name("lowerSnake")

    UPPER_SNAKE = hydra.core.Name("upperSnake")

CaseConvention.TYPE_ = hydra.core.Name("hydra.util.CaseConvention")

@dataclass(frozen=True)
class Coder(Generic[V1, V2]):
    r"""An encoder and decoder; a bidirectional transformation between two types."""

    encode: Annotated[Callable[[hydra.context.Context, V1], Either[hydra.context.InContext[hydra.error.Error], V2]], "A function which encodes source values as target values in a given context"]
    decode: Annotated[Callable[[hydra.context.Context, V2], Either[hydra.context.InContext[hydra.error.Error], V1]], "A function which decodes target values as source values in a given context"]

    TYPE_ = hydra.core.Name("hydra.util.Coder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

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
