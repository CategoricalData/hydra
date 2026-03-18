# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.parsing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.phantoms

A = TypeVar("A")

def parse_error(message: hydra.phantoms.TTerm[str], remainder: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseError"), (hydra.core.Field(hydra.core.Name("message"), message.value), hydra.core.Field(hydra.core.Name("remainder"), remainder.value))))))

def parse_error_message(x: hydra.phantoms.TTerm[hydra.parsing.ParseError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseError"), hydra.core.Name("message")))))))), x.value))))

def parse_error_remainder(x: hydra.phantoms.TTerm[hydra.parsing.ParseError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseError"), hydra.core.Name("remainder")))))))), x.value))))

def parse_error_with_message(original: hydra.phantoms.TTerm[hydra.parsing.ParseError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseError"), (hydra.core.Field(hydra.core.Name("message"), new_val.value), hydra.core.Field(hydra.core.Name("remainder"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseError"), hydra.core.Name("remainder")))))))), original.value)))))))))

def parse_error_with_remainder(original: hydra.phantoms.TTerm[hydra.parsing.ParseError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseError"), (hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseError"), hydra.core.Name("message")))))))), original.value)))), hydra.core.Field(hydra.core.Name("remainder"), new_val.value))))))

def parse_result_failure(x: hydra.phantoms.TTerm[hydra.parsing.ParseError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.parsing.ParseResult"), hydra.core.Field(hydra.core.Name("failure"), x.value)))))

def parse_result_success(x: hydra.phantoms.TTerm[hydra.parsing.ParseSuccess[A]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.parsing.ParseResult"), hydra.core.Field(hydra.core.Name("success"), x.value)))))

def parse_success(value: hydra.phantoms.TTerm[A], remainder: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseSuccess"), (hydra.core.Field(hydra.core.Name("value"), value.value), hydra.core.Field(hydra.core.Name("remainder"), remainder.value))))))

def parse_success_remainder(x: hydra.phantoms.TTerm[hydra.parsing.ParseSuccess[A]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseSuccess"), hydra.core.Name("remainder")))))))), x.value))))

def parse_success_value(x: hydra.phantoms.TTerm[hydra.parsing.ParseSuccess[A]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseSuccess"), hydra.core.Name("value")))))))), x.value))))

def parse_success_with_remainder(original: hydra.phantoms.TTerm[hydra.parsing.ParseSuccess[A]], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseSuccess"), (hydra.core.Field(hydra.core.Name("value"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseSuccess"), hydra.core.Name("value")))))))), original.value)))), hydra.core.Field(hydra.core.Name("remainder"), new_val.value))))))

def parse_success_with_value(original: hydra.phantoms.TTerm[hydra.parsing.ParseSuccess[A]], new_val: hydra.phantoms.TTerm[A]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.parsing.ParseSuccess"), (hydra.core.Field(hydra.core.Name("value"), new_val.value), hydra.core.Field(hydra.core.Name("remainder"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.parsing.ParseSuccess"), hydra.core.Name("remainder")))))))), original.value)))))))))

def parser(x: hydra.phantoms.TTerm[Callable[[str], hydra.parsing.ParseResult[A]]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.parsing.Parser"), x.value))))

def un_parser(x: hydra.phantoms.TTerm[hydra.parsing.Parser[A]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.parsing.Parser"))))))), x.value))))
