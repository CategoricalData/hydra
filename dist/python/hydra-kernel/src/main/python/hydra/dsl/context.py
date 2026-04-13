# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.context."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import FrozenDict, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.phantoms

E = TypeVar("E")

def context(trace: hydra.phantoms.TTerm[frozenlist[str]], messages: hydra.phantoms.TTerm[frozenlist[str]], other: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.Context"), (hydra.core.Field(hydra.core.Name("trace"), trace.value), hydra.core.Field(hydra.core.Name("messages"), messages.value), hydra.core.Field(hydra.core.Name("other"), other.value))))))

def context_messages(x: hydra.phantoms.TTerm[hydra.context.Context]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("messages")))))))), x.value))))

def context_other(x: hydra.phantoms.TTerm[hydra.context.Context]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("other")))))))), x.value))))

def context_trace(x: hydra.phantoms.TTerm[hydra.context.Context]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("trace")))))))), x.value))))

def context_with_messages(original: hydra.phantoms.TTerm[hydra.context.Context], new_val: hydra.phantoms.TTerm[frozenlist[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.Context"), (hydra.core.Field(hydra.core.Name("trace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("trace")))))))), original.value)))), hydra.core.Field(hydra.core.Name("messages"), new_val.value), hydra.core.Field(hydra.core.Name("other"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("other")))))))), original.value)))))))))

def context_with_other(original: hydra.phantoms.TTerm[hydra.context.Context], new_val: hydra.phantoms.TTerm[FrozenDict[hydra.core.Name, hydra.core.Term]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.Context"), (hydra.core.Field(hydra.core.Name("trace"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("trace")))))))), original.value)))), hydra.core.Field(hydra.core.Name("messages"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("messages")))))))), original.value)))), hydra.core.Field(hydra.core.Name("other"), new_val.value))))))

def context_with_trace(original: hydra.phantoms.TTerm[hydra.context.Context], new_val: hydra.phantoms.TTerm[frozenlist[str]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.Context"), (hydra.core.Field(hydra.core.Name("trace"), new_val.value), hydra.core.Field(hydra.core.Name("messages"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("messages")))))))), original.value)))), hydra.core.Field(hydra.core.Name("other"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.Context"), hydra.core.Name("other")))))))), original.value)))))))))

def in_context(object: hydra.phantoms.TTerm[E], context: hydra.phantoms.TTerm[hydra.context.Context]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.InContext"), (hydra.core.Field(hydra.core.Name("object"), object.value), hydra.core.Field(hydra.core.Name("context"), context.value))))))

def in_context_context(x: hydra.phantoms.TTerm[hydra.context.InContext[E]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.InContext"), hydra.core.Name("context")))))))), x.value))))

def in_context_object(x: hydra.phantoms.TTerm[hydra.context.InContext[E]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.InContext"), hydra.core.Name("object")))))))), x.value))))

def in_context_with_context(original: hydra.phantoms.TTerm[hydra.context.InContext[E]], new_val: hydra.phantoms.TTerm[hydra.context.Context]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.InContext"), (hydra.core.Field(hydra.core.Name("object"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.InContext"), hydra.core.Name("object")))))))), original.value)))), hydra.core.Field(hydra.core.Name("context"), new_val.value))))))

def in_context_with_object(original: hydra.phantoms.TTerm[hydra.context.InContext[E]], new_val: hydra.phantoms.TTerm[E]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.context.InContext"), (hydra.core.Field(hydra.core.Name("object"), new_val.value), hydra.core.Field(hydra.core.Name("context"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.context.InContext"), hydra.core.Name("context")))))))), original.value)))))))))
