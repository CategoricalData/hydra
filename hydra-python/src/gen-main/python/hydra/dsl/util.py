# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.util."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either
from typing import TypeVar, cast
import hydra.core
import hydra.phantoms

T1 = TypeVar("T1")
T2 = TypeVar("T2")
V1 = TypeVar("V1")
V2 = TypeVar("V2")

def adapter(is_lossy: hydra.phantoms.TTerm[bool], source: hydra.phantoms.TTerm[T1], target: hydra.phantoms.TTerm[T2], coder: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Adapter"), (hydra.core.Field(hydra.core.Name("isLossy"), is_lossy.value), hydra.core.Field(hydra.core.Name("source"), source.value), hydra.core.Field(hydra.core.Name("target"), target.value), hydra.core.Field(hydra.core.Name("coder"), coder.value))))))

def adapter_coder(x: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("coder")))))))), x.value))))

def adapter_is_lossy(x: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("isLossy")))))))), x.value))))

def adapter_source(x: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("source")))))))), x.value))))

def adapter_target(x: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("target")))))))), x.value))))

def adapter_with_coder(original: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Adapter"), (hydra.core.Field(hydra.core.Name("isLossy"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("isLossy")))))))), original.value)))), hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("source")))))))), original.value)))), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("target")))))))), original.value)))), hydra.core.Field(hydra.core.Name("coder"), new_val.value))))))

def adapter_with_is_lossy(original: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Adapter"), (hydra.core.Field(hydra.core.Name("isLossy"), new_val.value), hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("source")))))))), original.value)))), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("target")))))))), original.value)))), hydra.core.Field(hydra.core.Name("coder"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("coder")))))))), original.value)))))))))

def adapter_with_source(original: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[T1]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Adapter"), (hydra.core.Field(hydra.core.Name("isLossy"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("isLossy")))))))), original.value)))), hydra.core.Field(hydra.core.Name("source"), new_val.value), hydra.core.Field(hydra.core.Name("target"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("target")))))))), original.value)))), hydra.core.Field(hydra.core.Name("coder"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("coder")))))))), original.value)))))))))

def adapter_with_target(original: hydra.phantoms.TTerm[hydra.util.Adapter[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[T2]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Adapter"), (hydra.core.Field(hydra.core.Name("isLossy"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("isLossy")))))))), original.value)))), hydra.core.Field(hydra.core.Name("source"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("source")))))))), original.value)))), hydra.core.Field(hydra.core.Name("target"), new_val.value), hydra.core.Field(hydra.core.Name("coder"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Adapter"), hydra.core.Name("coder")))))))), original.value)))))))))

def bicoder(encode: hydra.phantoms.TTerm[Callable[[T1], hydra.util.Adapter[T1, T2, V1, V2]]], decode: hydra.phantoms.TTerm[Callable[[T2], hydra.util.Adapter[T2, T1, V2, V1]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Bicoder"), (hydra.core.Field(hydra.core.Name("encode"), encode.value), hydra.core.Field(hydra.core.Name("decode"), decode.value))))))

def bicoder_decode(x: hydra.phantoms.TTerm[hydra.util.Bicoder[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Bicoder"), hydra.core.Name("decode")))))))), x.value))))

def bicoder_encode(x: hydra.phantoms.TTerm[hydra.util.Bicoder[T1, T2, V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Bicoder"), hydra.core.Name("encode")))))))), x.value))))

def bicoder_with_decode(original: hydra.phantoms.TTerm[hydra.util.Bicoder[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[Callable[[T2], hydra.util.Adapter[T2, T1, V2, V1]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Bicoder"), (hydra.core.Field(hydra.core.Name("encode"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Bicoder"), hydra.core.Name("encode")))))))), original.value)))), hydra.core.Field(hydra.core.Name("decode"), new_val.value))))))

def bicoder_with_encode(original: hydra.phantoms.TTerm[hydra.util.Bicoder[T1, T2, V1, V2]], new_val: hydra.phantoms.TTerm[Callable[[T1], hydra.util.Adapter[T1, T2, V1, V2]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Bicoder"), (hydra.core.Field(hydra.core.Name("encode"), new_val.value), hydra.core.Field(hydra.core.Name("decode"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Bicoder"), hydra.core.Name("decode")))))))), original.value)))))))))

case_convention_camel = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("camel"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_lower_snake = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("lowerSnake"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_pascal = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("pascal"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

case_convention_upper_snake = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.CaseConvention"), hydra.core.Field(hydra.core.Name("upperSnake"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def coder(encode: hydra.phantoms.TTerm[Callable[[hydra.context.Context, V1], Either[hydra.context.InContext[hydra.errors.Error], V2]]], decode: hydra.phantoms.TTerm[Callable[[hydra.context.Context, V2], Either[hydra.context.InContext[hydra.errors.Error], V1]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Coder"), (hydra.core.Field(hydra.core.Name("encode"), encode.value), hydra.core.Field(hydra.core.Name("decode"), decode.value))))))

def coder_decode(x: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Coder"), hydra.core.Name("decode")))))))), x.value))))

def coder_encode(x: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Coder"), hydra.core.Name("encode")))))))), x.value))))

def coder_with_decode(original: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]], new_val: hydra.phantoms.TTerm[Callable[[hydra.context.Context, V2], Either[hydra.context.InContext[hydra.errors.Error], V1]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Coder"), (hydra.core.Field(hydra.core.Name("encode"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Coder"), hydra.core.Name("encode")))))))), original.value)))), hydra.core.Field(hydra.core.Name("decode"), new_val.value))))))

def coder_with_encode(original: hydra.phantoms.TTerm[hydra.util.Coder[V1, V2]], new_val: hydra.phantoms.TTerm[Callable[[hydra.context.Context, V1], Either[hydra.context.InContext[hydra.errors.Error], V2]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.util.Coder"), (hydra.core.Field(hydra.core.Name("encode"), new_val.value), hydra.core.Field(hydra.core.Name("decode"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.util.Coder"), hydra.core.Name("decode")))))))), original.value)))))))))

comparison_equal_to = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("equalTo"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_greater_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("greaterThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

comparison_less_than = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Comparison"), hydra.core.Field(hydra.core.Name("lessThan"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

precision_arbitrary = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("arbitrary"), cast(hydra.core.Term, hydra.core.TermUnit()))))))

def precision_bits(x: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.util.Precision"), hydra.core.Field(hydra.core.Name("bits"), x.value)))))
