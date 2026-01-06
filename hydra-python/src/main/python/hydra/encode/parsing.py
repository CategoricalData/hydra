# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.parsing."""

from __future__ import annotations
from collections.abc import Callable
from typing import TypeVar, cast
import hydra.core
import hydra.parsing

T0 = TypeVar("T0")

def parse_error(x: hydra.parsing.ParseError) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseError"), (hydra.core.Field(hydra.core.Name("message"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.message)), hydra.core.Field(hydra.core.Name("remainder"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.remainder))))))

def parse_success(a: Callable[[T0], hydra.core.Term], x: hydra.parsing.ParseSuccess[T0]) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseSuccess"), (hydra.core.Field(hydra.core.Name("value"), a(x.value)), hydra.core.Field(hydra.core.Name("remainder"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.remainder))))))

def parse_result(a: Callable[[T0], hydra.core.Term], v1: hydra.parsing.ParseResult[T0]) -> hydra.core.Type:
    match v1:
        case hydra.parsing.ParseResultSuccess(value=v):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.parsing.ParseResult"), hydra.core.Field(hydra.core.Name("success"), parse_success(a, v)))))
        
        case hydra.parsing.ParseResultFailure(value=v2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.parsing.ParseResult"), hydra.core.Field(hydra.core.Name("failure"), parse_error(v2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
