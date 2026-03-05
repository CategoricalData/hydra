# Note: this is an automatically generated file. Do not edit.

r"""Abstractions for single- and bidirectional transformations."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Either
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
    
    TYPE_ = hydra.core.Name("hydra.compute.Adapter")
    IS_LOSSY = hydra.core.Name("isLossy")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")
    CODER = hydra.core.Name("coder")

@dataclass(frozen=True)
class Bicoder(Generic[T1, T2, V1, V2]):
    r"""A two-level encoder and decoder, operating both at a type level and an instance (data) level."""
    
    encode: Annotated[Callable[[T1], Adapter[T1, T2, V1, V2]], "A function from source types to adapters"]
    decode: Annotated[Callable[[T2], Adapter[T2, T1, V2, V1]], "A function from target types to adapters"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Bicoder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

@dataclass(frozen=True)
class Coder(Generic[V1, V2]):
    r"""An encoder and decoder; a bidirectional transformation between two types."""
    
    encode: Annotated[Callable[[hydra.context.Context, V1], Either[hydra.context.InContext[hydra.error.OtherError], V2]], "A function which encodes source values as target values in a given context"]
    decode: Annotated[Callable[[hydra.context.Context, V2], Either[hydra.context.InContext[hydra.error.OtherError], V1]], "A function which decodes target values as source values in a given context"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Coder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")
