# Note: this is an automatically generated file. Do not edit.

r"""Execution context for tracing and diagnostics."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar, cast
import hydra.core

E = TypeVar("E")

@dataclass(frozen=True)
class Context:
    r"""An execution context for tracing and diagnostics, threaded through function calls."""
    
    trace: Annotated[frozenlist[str], "A stack of context labels describing the current execution path"]
    messages: Annotated[frozenlist[str], "A log of warnings and/or info messages"]
    other: Annotated[FrozenDict[hydra.core.Name, hydra.core.Term], "A map of string keys to arbitrary terms as values, for application-specific use"]
    
    TYPE_ = hydra.core.Name("hydra.context.Context")
    TRACE = hydra.core.Name("trace")
    MESSAGES = hydra.core.Name("messages")
    OTHER = hydra.core.Name("other")

@dataclass(frozen=True)
class InContext(Generic[E]):
    r"""A particular domain object (such as an error) together with an execution context."""
    
    object: Annotated[E, "A domain object; typically an error"]
    context: Annotated[Context, "The execution context at the point of capture"]
    
    TYPE_ = hydra.core.Name("hydra.context.InContext")
    OBJECT = hydra.core.Name("object")
    CONTEXT = hydra.core.Name("context")
