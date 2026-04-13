# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.errors."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

def decoding_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.DecodingError"), x.value))))

def error_checking(x: hydra.phantoms.TTerm[hydra.error.checking.CheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("checking"), x.value)))))

def error_decoding(x: hydra.phantoms.TTerm[hydra.errors.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("decoding"), x.value)))))

def error_duplicate_binding(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateBinding"), x.value)))))

def error_duplicate_field(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateField"), x.value)))))

def error_extraction(x: hydra.phantoms.TTerm[hydra.errors.ExtractionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("extraction"), x.value)))))

def error_inference(x: hydra.phantoms.TTerm[hydra.errors.InferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("inference"), x.value)))))

def error_other(x: hydra.phantoms.TTerm[hydra.errors.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def error_resolution(x: hydra.phantoms.TTerm[hydra.errors.ResolutionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("resolution"), x.value)))))

def error_undefined_field(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedField"), x.value)))))

def error_undefined_term_variable(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), x.value)))))

def error_unexpected_term_variant(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTermVariant"), x.value)))))

def error_unexpected_type_variant(x: hydra.phantoms.TTerm[hydra.error.core.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTypeVariant"), x.value)))))

def error_unification(x: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unification"), x.value)))))

def error_untyped_term_variable(x: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), x.value)))))

def extraction_error_empty_list(x: hydra.phantoms.TTerm[hydra.errors.EmptyListError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("emptyList"), x.value)))))

def extraction_error_multiple_bindings(x: hydra.phantoms.TTerm[hydra.errors.MultipleBindingsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("multipleBindings"), x.value)))))

def extraction_error_multiple_fields(x: hydra.phantoms.TTerm[hydra.errors.MultipleFieldsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("multipleFields"), x.value)))))

def extraction_error_no_matching_field(x: hydra.phantoms.TTerm[hydra.errors.NoMatchingFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("noMatchingField"), x.value)))))

def extraction_error_no_such_binding(x: hydra.phantoms.TTerm[hydra.errors.NoSuchBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("noSuchBinding"), x.value)))))

def extraction_error_not_enough_cases(x: hydra.phantoms.TTerm[hydra.errors.NotEnoughCasesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("notEnoughCases"), x.value)))))

def extraction_error_unexpected_shape(x: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("unexpectedShape"), x.value)))))

def inference_error_checking(x: hydra.phantoms.TTerm[hydra.error.checking.CheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("checking"), x.value)))))

def inference_error_other(x: hydra.phantoms.TTerm[hydra.errors.OtherInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def inference_error_unification(x: hydra.phantoms.TTerm[hydra.errors.UnificationInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("unification"), x.value)))))

def multiple_bindings_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleBindingsError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def multiple_bindings_error_name(x: hydra.phantoms.TTerm[hydra.errors.MultipleBindingsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.MultipleBindingsError"), hydra.core.Name("name")))))))), x.value))))

def multiple_bindings_error_with_name(original: hydra.phantoms.TTerm[hydra.errors.MultipleBindingsError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleBindingsError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def multiple_fields_error(field_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleFieldsError"), (hydra.core.Field(hydra.core.Name("fieldName"), field_name.value),)))))

def multiple_fields_error_field_name(x: hydra.phantoms.TTerm[hydra.errors.MultipleFieldsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.MultipleFieldsError"), hydra.core.Name("fieldName")))))))), x.value))))

def multiple_fields_error_with_field_name(original: hydra.phantoms.TTerm[hydra.errors.MultipleFieldsError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleFieldsError"), (hydra.core.Field(hydra.core.Name("fieldName"), new_val.value),)))))

def no_matching_field_error(field_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoMatchingFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), field_name.value),)))))

def no_matching_field_error_field_name(x: hydra.phantoms.TTerm[hydra.errors.NoMatchingFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.NoMatchingFieldError"), hydra.core.Name("fieldName")))))))), x.value))))

def no_matching_field_error_with_field_name(original: hydra.phantoms.TTerm[hydra.errors.NoMatchingFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoMatchingFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), new_val.value),)))))

def no_such_binding_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchBindingError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def no_such_binding_error_name(x: hydra.phantoms.TTerm[hydra.errors.NoSuchBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.NoSuchBindingError"), hydra.core.Name("name")))))))), x.value))))

def no_such_binding_error_with_name(original: hydra.phantoms.TTerm[hydra.errors.NoSuchBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchBindingError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def no_such_primitive_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def no_such_primitive_error_name(x: hydra.phantoms.TTerm[hydra.errors.NoSuchPrimitiveError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), hydra.core.Name("name")))))))), x.value))))

def no_such_primitive_error_with_name(original: hydra.phantoms.TTerm[hydra.errors.NoSuchPrimitiveError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def other_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.OtherError"), x.value))))

def other_inference_error(path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], message: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.OtherInferenceError"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("message"), message.value))))))

def other_inference_error_message(x: hydra.phantoms.TTerm[hydra.errors.OtherInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.OtherInferenceError"), hydra.core.Name("message")))))))), x.value))))

def other_inference_error_path(x: hydra.phantoms.TTerm[hydra.errors.OtherInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.OtherInferenceError"), hydra.core.Name("path")))))))), x.value))))

def other_inference_error_with_message(original: hydra.phantoms.TTerm[hydra.errors.OtherInferenceError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.OtherInferenceError"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.OtherInferenceError"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), new_val.value))))))

def other_inference_error_with_path(original: hydra.phantoms.TTerm[hydra.errors.OtherInferenceError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.OtherInferenceError"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.OtherInferenceError"), hydra.core.Name("message")))))))), original.value)))))))))

def other_resolution_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.OtherResolutionError"), x.value))))

def resolution_error_no_matching_field(x: hydra.phantoms.TTerm[hydra.errors.NoMatchingFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noMatchingField"), x.value)))))

def resolution_error_no_such_binding(x: hydra.phantoms.TTerm[hydra.errors.NoSuchBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noSuchBinding"), x.value)))))

def resolution_error_no_such_primitive(x: hydra.phantoms.TTerm[hydra.errors.NoSuchPrimitiveError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noSuchPrimitive"), x.value)))))

def resolution_error_other(x: hydra.phantoms.TTerm[hydra.errors.OtherResolutionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def resolution_error_unexpected_shape(x: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("unexpectedShape"), x.value)))))

def un_decoding_error(x: hydra.phantoms.TTerm[hydra.errors.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.errors.DecodingError"))))))), x.value))))

def un_other_error(x: hydra.phantoms.TTerm[hydra.errors.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.errors.OtherError"))))))), x.value))))

def un_other_resolution_error(x: hydra.phantoms.TTerm[hydra.errors.OtherResolutionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.errors.OtherResolutionError"))))))), x.value))))

def unexpected_shape_error(expected: hydra.phantoms.TTerm[str], actual: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnexpectedShapeError"), (hydra.core.Field(hydra.core.Name("expected"), expected.value), hydra.core.Field(hydra.core.Name("actual"), actual.value))))))

def unexpected_shape_error_actual(x: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnexpectedShapeError"), hydra.core.Name("actual")))))))), x.value))))

def unexpected_shape_error_expected(x: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnexpectedShapeError"), hydra.core.Name("expected")))))))), x.value))))

def unexpected_shape_error_with_actual(original: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnexpectedShapeError"), (hydra.core.Field(hydra.core.Name("expected"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnexpectedShapeError"), hydra.core.Name("expected")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actual"), new_val.value))))))

def unexpected_shape_error_with_expected(original: hydra.phantoms.TTerm[hydra.errors.UnexpectedShapeError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnexpectedShapeError"), (hydra.core.Field(hydra.core.Name("expected"), new_val.value), hydra.core.Field(hydra.core.Name("actual"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnexpectedShapeError"), hydra.core.Name("actual")))))))), original.value)))))))))

def unification_error(left_type: hydra.phantoms.TTerm[hydra.core.Type], right_type: hydra.phantoms.TTerm[hydra.core.Type], message: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), left_type.value), hydra.core.Field(hydra.core.Name("rightType"), right_type.value), hydra.core.Field(hydra.core.Name("message"), message.value))))))

def unification_error_left_type(x: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("leftType")))))))), x.value))))

def unification_error_message(x: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("message")))))))), x.value))))

def unification_error_right_type(x: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("rightType")))))))), x.value))))

def unification_error_with_left_type(original: hydra.phantoms.TTerm[hydra.errors.UnificationError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), new_val.value), hydra.core.Field(hydra.core.Name("rightType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("rightType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("message")))))))), original.value)))))))))

def unification_error_with_message(original: hydra.phantoms.TTerm[hydra.errors.UnificationError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("leftType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rightType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("rightType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), new_val.value))))))

def unification_error_with_right_type(original: hydra.phantoms.TTerm[hydra.errors.UnificationError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("leftType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("rightType"), new_val.value), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationError"), hydra.core.Name("message")))))))), original.value)))))))))

def unification_inference_error(path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], cause: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationInferenceError"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("cause"), cause.value))))))

def unification_inference_error_cause(x: hydra.phantoms.TTerm[hydra.errors.UnificationInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationInferenceError"), hydra.core.Name("cause")))))))), x.value))))

def unification_inference_error_path(x: hydra.phantoms.TTerm[hydra.errors.UnificationInferenceError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationInferenceError"), hydra.core.Name("path")))))))), x.value))))

def unification_inference_error_with_cause(original: hydra.phantoms.TTerm[hydra.errors.UnificationInferenceError], new_val: hydra.phantoms.TTerm[hydra.errors.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationInferenceError"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationInferenceError"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("cause"), new_val.value))))))

def unification_inference_error_with_path(original: hydra.phantoms.TTerm[hydra.errors.UnificationInferenceError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationInferenceError"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("cause"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.errors.UnificationInferenceError"), hydra.core.Name("cause")))))))), original.value)))))))))
