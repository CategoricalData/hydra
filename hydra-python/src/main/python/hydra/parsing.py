# Note: this is an automatically generated file. Do not edit.

r"""Parser combinator types for text parsing."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from hydra.dsl.python import Node
from typing import Annotated, Any, Generic, TypeAlias, TypeVar
import hydra.core

A = TypeVar("A")

@dataclass(frozen=True)
class ParseError:
    r"""An error which occurred while parsing."""
    
    message: Annotated[str, "An error message"]
    remainder: Annotated[str, "The remaining input at the point of failure"]

PARSE_ERROR__NAME = hydra.core.Name("hydra.parsing.ParseError")
PARSE_ERROR__MESSAGE__NAME = hydra.core.Name("message")
PARSE_ERROR__REMAINDER__NAME = hydra.core.Name("remainder")

class ParseResultSuccess(Node["ParseSuccess[A]"]):
    r"""A successful parse, with a value and the remaining unparsed input."""

class ParseResultFailure(Node["ParseError"]):
    r"""A failed parse, with an error message and the remaining input."""

# ParseResult needs to be subscriptable at runtime for cast() compatibility in Python 3.10/PyPy
class _ParseResultMeta(type):
    """Metaclass that makes ParseResult subscriptable at runtime."""
    def __getitem__(cls, item: Any) -> Any:
        return object

class ParseResult(metaclass=_ParseResultMeta):
    """A type alias for parse results (ParseResultSuccess[A] | ParseResultFailure). Subscriptable at runtime for cast() compatibility."""
    pass

PARSE_RESULT__NAME = hydra.core.Name("hydra.parsing.ParseResult")
PARSE_RESULT__SUCCESS__NAME = hydra.core.Name("success")
PARSE_RESULT__FAILURE__NAME = hydra.core.Name("failure")

@dataclass(frozen=True)
class ParseSuccess(Generic[A]):
    r"""A successful parse result."""
    
    value: Annotated[A, "The parsed value"]
    remainder: Annotated[str, "The remaining unparsed input"]

PARSE_SUCCESS__NAME = hydra.core.Name("hydra.parsing.ParseSuccess")
PARSE_SUCCESS__VALUE__NAME = hydra.core.Name("value")
PARSE_SUCCESS__REMAINDER__NAME = hydra.core.Name("remainder")

class Parser(Node["Callable[[str], ParseResult[A]]"], Generic[A]):
    r"""A parser which consumes characters from a string and produces a value."""

PARSER__NAME = hydra.core.Name("hydra.parsing.Parser")
