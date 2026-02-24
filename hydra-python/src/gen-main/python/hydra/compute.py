# Note: this is an automatically generated file. Do not edit.

r"""Abstractions for single- and bidirectional transformations."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core

S = TypeVar("S")
S1 = TypeVar("S1")
S2 = TypeVar("S2")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
V = TypeVar("V")
V1 = TypeVar("V1")
V2 = TypeVar("V2")

@dataclass(frozen=True)
class Adapter(Generic[S1, S2, T1, T2, V1, V2]):
    r"""A two-level bidirectional encoder which adapts types to types and terms to terms."""
    
    is_lossy: Annotated[bool, "Whether information may be lost in the course of this adaptation"]
    source: Annotated[T1, "The source type"]
    target: Annotated[T2, "The target type"]
    coder: Annotated[Coder[S1, S2, V1, V2], "The coder for transforming instances of the source type to instances of the target type"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Adapter")
    IS_LOSSY = hydra.core.Name("isLossy")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")
    CODER = hydra.core.Name("coder")

@dataclass(frozen=True)
class Bicoder(Generic[S1, S2, T1, T2, V1, V2]):
    r"""A two-level encoder and decoder, operating both at a type level and an instance (data) level."""
    
    encode: Annotated[Callable[[T1], Adapter[S1, S2, T1, T2, V1, V2]], "A function from source types to adapters"]
    decode: Annotated[Callable[[T2], Adapter[S2, S1, T2, T1, V2, V1]], "A function from target types to adapters"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Bicoder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

@dataclass(frozen=True)
class Coder(Generic[S1, S2, V1, V2]):
    r"""An encoder and decoder; a bidirectional flow between two types."""
    
    encode: Annotated[Callable[[V1], Flow[S1, V2]], "A function from source values to a flow of target values"]
    decode: Annotated[Callable[[V2], Flow[S2, V1]], "A function from target values to a flow of source values"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Coder")
    ENCODE = hydra.core.Name("encode")
    DECODE = hydra.core.Name("decode")

class Flow(Node["Callable[[S, Trace], FlowState[S, V]]"], Generic[S, V]):
    r"""A variant of the State monad with built-in logging and error handling."""

Flow.TYPE_ = hydra.core.Name("hydra.compute.Flow")

@dataclass(frozen=True)
class FlowState(Generic[S, V]):
    r"""The result of evaluating a Flow."""
    
    value: Annotated[Maybe[V], "The resulting value, or nothing in the case of failure"]
    state: Annotated[S, "The final state"]
    trace: Annotated[Trace, "The trace (log) produced during evaluation"]
    
    TYPE_ = hydra.core.Name("hydra.compute.FlowState")
    VALUE = hydra.core.Name("value")
    STATE = hydra.core.Name("state")
    TRACE = hydra.core.Name("trace")

@dataclass(frozen=True)
class Trace:
    r"""A container for logging and error information."""
    
    stack: Annotated[frozenlist[str], "A stack of context labels"]
    messages: Annotated[frozenlist[str], "A log of warnings and/or info messages"]
    other: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "A map of string keys to arbitrary terms as values, for application-specific use"]
    
    TYPE_ = hydra.core.Name("hydra.compute.Trace")
    STACK = hydra.core.Name("stack")
    MESSAGES = hydra.core.Name("messages")
    OTHER = hydra.core.Name("other")
