# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error.core."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

def duplicate_binding_error(location: hydra.phantoms.TTerm[hydra.accessors.AccessorPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_binding_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_binding_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_binding_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError], new_val: hydra.phantoms.TTerm[hydra.accessors.AccessorPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_binding_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def duplicate_field_error(location: hydra.phantoms.TTerm[hydra.accessors.AccessorPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_field_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_field_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_field_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError], new_val: hydra.phantoms.TTerm[hydra.accessors.AccessorPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_field_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def invalid_term_error_duplicate_binding(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateBinding"), x.value)))))

def invalid_term_error_duplicate_field(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateField"), x.value)))))

def undefined_field_error(field_name: hydra.phantoms.TTerm[hydra.core.Name], type_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), field_name.value), hydra.core.Field(hydra.core.Name("typeName"), type_name.value))))))

def undefined_field_error_field_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.core.Name("fieldName")))))))), x.value))))

def undefined_field_error_type_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.core.Name("typeName")))))))), x.value))))

def undefined_field_error_with_field_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), new_val.value), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.core.Name("typeName")))))))), original.value)))))))))

def undefined_field_error_with_type_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedFieldError"), hydra.core.Name("fieldName")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), new_val.value))))))

def undefined_term_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def undefined_term_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTermError"), hydra.core.Name("name")))))))), x.value))))

def undefined_term_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def undefined_type_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def undefined_type_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def unexpected_term_variant_error(expected_variant: hydra.phantoms.TTerm[hydra.variants.TermVariant], actual_term: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), expected_variant.value), hydra.core.Field(hydra.core.Name("actualTerm"), actual_term.value))))))

def unexpected_term_variant_error_actual_term(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.core.Name("actualTerm")))))))), x.value))))

def unexpected_term_variant_error_expected_variant(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.core.Name("expectedVariant")))))))), x.value))))

def unexpected_term_variant_error_with_actual_term(original: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.core.Name("expectedVariant")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualTerm"), new_val.value))))))

def unexpected_term_variant_error_with_expected_variant(original: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), new_val.value), hydra.core.Field(hydra.core.Name("actualTerm"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), hydra.core.Name("actualTerm")))))))), original.value)))))))))

def unexpected_type_variant_error(expected_variant: hydra.phantoms.TTerm[hydra.variants.TypeVariant], actual_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), expected_variant.value), hydra.core.Field(hydra.core.Name("actualType"), actual_type.value))))))

def unexpected_type_variant_error_actual_type(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.core.Name("actualType")))))))), x.value))))

def unexpected_type_variant_error_expected_variant(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.core.Name("expectedVariant")))))))), x.value))))

def unexpected_type_variant_error_with_actual_type(original: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTypeVariantError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.core.Name("expectedVariant")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualType"), new_val.value))))))

def unexpected_type_variant_error_with_expected_variant(original: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTypeVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TypeVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), new_val.value), hydra.core.Field(hydra.core.Name("actualType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), hydra.core.Name("actualType")))))))), original.value)))))))))
