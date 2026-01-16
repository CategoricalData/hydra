# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.util."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.util

def case_convention(v1: hydra.util.CaseConvention) -> hydra.core.Type:
    match v1:
        case hydra.util.CaseConvention.CAMEL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("camel"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.CaseConvention.PASCAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("pascal"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.CaseConvention.LOWER_SNAKE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("lowerSnake"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.CaseConvention.UPPER_SNAKE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("upperSnake"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def comparison(v1: hydra.util.Comparison) -> hydra.core.Type:
    match v1:
        case hydra.util.Comparison.LESS_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("lessThan"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.Comparison.EQUAL_TO:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("equalTo"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.Comparison.GREATER_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("greaterThan"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def decoding_error(x: hydra.util.DecodingError) -> hydra.core.Type:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.util.DecodingError"), (lambda x2: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x2)))))(x.value))))

def precision(v1: hydra.util.Precision) -> hydra.core.Type:
    match v1:
        case hydra.util.PrecisionArbitrary():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("arbitrary"), (lambda _: cast(hydra.core.Term, hydra.core.TermUnit()))(None)))))
        
        case hydra.util.PrecisionBits(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("bits"), (lambda x: cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x)))))))(y2)))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
