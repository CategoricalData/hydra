# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.util."""

from __future__ import annotations
from typing import cast
import hydra.core
import hydra.util

def case_convention(v1: hydra.util.CaseConvention) -> hydra.core.Term:
    match v1:
        case hydra.util.CaseConvention.CAMEL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("camel"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.CaseConvention.PASCAL:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("pascal"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.CaseConvention.LOWER_SNAKE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("lowerSnake"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.CaseConvention.UPPER_SNAKE:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("upperSnake"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def comparison(v1: hydra.util.Comparison) -> hydra.core.Term:
    match v1:
        case hydra.util.Comparison.LESS_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("lessThan"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.Comparison.EQUAL_TO:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("equalTo"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.Comparison.GREATER_THAN:
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("greaterThan"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")

def decoding_error(x: hydra.util.DecodingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.util.DecodingError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def precision(v1: hydra.util.Precision) -> hydra.core.Term:
    match v1:
        case hydra.util.PrecisionArbitrary():
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("arbitrary"), cast(hydra.core.Term, hydra.core.TermUnit())))))
        
        case hydra.util.PrecisionBits(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("bits"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(y2))))))))))
        
        case _:
            raise AssertionError("Unreachable: all variants handled")
