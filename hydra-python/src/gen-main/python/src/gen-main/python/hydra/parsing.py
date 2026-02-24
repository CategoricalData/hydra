# Note: this is an automatically generated file. Do not edit.

r"""Parser combinator types for text parsing."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import Node
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.core

A = TypeVar("A")

@dataclass(frozen=True)
class ParseError:
    r"""An error which occurred while parsing."""
    
    message: Annotated[str, "An error message"]
    remainder: Annotated[str, "The remaining input at the point of failure"]
    
    TYPE_ = hydra.core.Name("hydra.parsing.ParseError")
    MESSAGE = hydra.core.Name("message")
    REMAINDER = hydra.core.Name("remainder")

class ParseResultSuccess(Node["ParseSuccess[A]"]):
    r"""A successful parse, with a value and the remaining unparsed input"""

class ParseResultFailure(Node["ParseError"]):
    r"""A failed parse, with an error message and the remaining input"""

class _ParseResultMeta(type):
    def __getitem__(cls, item):
        return object

# The result of a parse operation.
class ParseResult(metaclass=_ParseResultMeta):
    r"""ParseResultSuccess[A] | ParseResultFailure"""
    
    TYPE_ = hydra.core.Name("hydra.parsing.ParseResult")
    SUCCESS = hydra.core.Name("success")
    FAILURE = hydra.core.Name("failure")

@dataclass(frozen=True)
class ParseSuccess(Generic[A]):
    r"""A successful parse result."""
    
    value: Annotated[A, "The parsed value"]
    remainder: Annotated[str, "The remaining unparsed input"]
    
    TYPE_ = hydra.core.Name("hydra.parsing.ParseSuccess")
    VALUE = hydra.core.Name("value")
    REMAINDER = hydra.core.Name("remainder")

class Parser(Node["Callable[[str], ParseResult[A]]"], Generic[A]):
    r"""A parser which consumes characters from a string and produces a value."""

Parser.TYPE_ = hydra.core.Name("hydra.parsing.Parser")
