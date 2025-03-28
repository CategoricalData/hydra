"""Abstractions for single- and bidirectional transformations."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import FrozenDict, frozenlist, Node
from typing import Annotated, Generic, TypeVar
import hydra.core

S = TypeVar("S")
S1 = TypeVar("S1")
S2 = TypeVar("S2")
T1 = TypeVar("T1")
T2 = TypeVar("T2")
V = TypeVar("V")
V1 = TypeVar("V1")
V2 = TypeVar("V2")

@dataclass
class Adapter(Generic[S1, S2, T1, T2, V1, V2]):
    """A two-level bidirectional encoder which adapts types to types and terms to terms."""
    
    is_lossy: bool
    source: T1
    target: T2
    coder: Coder[S1, S2, V1, V2]

@dataclass
class Bicoder(Generic[S1, S2, T1, T2, V1, V2]):
    """A two-level encoder and decoder, operating both at a type level and an instance (data) level."""
    
    encode: Callable[[T1], Adapter[S1, S2, T1, T2, V1, V2]]
    decode: Callable[[T2], Adapter[S2, S1, T2, T1, V2, V1]]

@dataclass
class Coder(Generic[S1, S2, V1, V2]):
    """An encoder and decoder; a bidirectional flow between two types."""
    
    encode: Callable[[V1], Flow[S1, V2]]
    decode: Callable[[V2], Flow[S2, V1]]

class Flow(Node["Callable[[S, Trace], FlowState[S, V]]"], Generic[S, V]):
    """A variant of the State monad with built-in logging and error handling."""

@dataclass
class FlowState(Generic[S, V]):
    """The result of evaluating a Flow."""
    
    value: V | None
    state: S
    trace: Trace

@dataclass
class Trace:
    """A container for logging and error information."""
    
    stack: frozenlist[str]
    messages: frozenlist[str]
    other: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "A map of string keys to arbitrary terms as values, for application-specific use"]
