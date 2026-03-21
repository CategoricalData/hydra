# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

def decoding_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.error.DecodingError"), x.value))))

def error_checking(x: hydra.phantoms.TTerm[hydra.errors.checking.CheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("checking"), x.value)))))

def error_decoding(x: hydra.phantoms.TTerm[hydra.error.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("decoding"), x.value)))))

def error_duplicate_binding(x: hydra.phantoms.TTerm[hydra.errors.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("duplicateBinding"), x.value)))))

def error_duplicate_field(x: hydra.phantoms.TTerm[hydra.errors.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("duplicateField"), x.value)))))

def error_other(x: hydra.phantoms.TTerm[hydra.error.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def error_undefined_field(x: hydra.phantoms.TTerm[hydra.errors.core.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedField"), x.value)))))

def error_undefined_term(x: hydra.phantoms.TTerm[hydra.errors.core.UndefinedTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedTerm"), x.value)))))

def error_undefined_type(x: hydra.phantoms.TTerm[hydra.errors.core.UndefinedTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedType"), x.value)))))

def error_unexpected_term_variant(x: hydra.phantoms.TTerm[hydra.errors.core.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unexpectedTermVariant"), x.value)))))

def error_unexpected_type_variant(x: hydra.phantoms.TTerm[hydra.errors.core.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unexpectedTypeVariant"), x.value)))))

def error_unification(x: hydra.phantoms.TTerm[hydra.error.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unification"), x.value)))))

def other_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.error.OtherError"), x.value))))

def un_decoding_error(x: hydra.phantoms.TTerm[hydra.error.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.error.DecodingError"))))))), x.value))))

def un_other_error(x: hydra.phantoms.TTerm[hydra.error.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.error.OtherError"))))))), x.value))))

def unification_error(left_type: hydra.phantoms.TTerm[hydra.core.Type], right_type: hydra.phantoms.TTerm[hydra.core.Type], message: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), left_type.value), hydra.core.Field(hydra.core.Name("rightType"), right_type.value), hydra.core.Field(hydra.core.Name("message"), message.value))))))

def unification_error_left_type(x: hydra.phantoms.TTerm[hydra.error.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("leftType")))))))), x.value))))

def unification_error_message(x: hydra.phantoms.TTerm[hydra.error.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("message")))))))), x.value))))

def unification_error_right_type(x: hydra.phantoms.TTerm[hydra.error.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("rightType")))))))), x.value))))

def unification_error_with_left_type(original: hydra.phantoms.TTerm[hydra.error.UnificationError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), new_val.value), hydra.core.Field(hydra.core.Name("rightType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("rightType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("message")))))))), original.value)))))))))

def unification_error_with_message(original: hydra.phantoms.TTerm[hydra.error.UnificationError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("leftType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rightType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("rightType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), new_val.value))))))

def unification_error_with_right_type(original: hydra.phantoms.TTerm[hydra.error.UnificationError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("leftType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rightType"), new_val.value), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnificationError"), hydra.core.Name("message")))))))), original.value)))))))))
