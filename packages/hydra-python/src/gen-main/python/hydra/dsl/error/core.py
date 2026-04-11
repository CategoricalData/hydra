# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error.core."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.phantoms

def constant_condition_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], value: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.ConstantConditionError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("value"), value.value))))))

def constant_condition_error_location(x: hydra.phantoms.TTerm[hydra.error.core.ConstantConditionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.core.Name("location")))))))), x.value))))

def constant_condition_error_value(x: hydra.phantoms.TTerm[hydra.error.core.ConstantConditionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.core.Name("value")))))))), x.value))))

def constant_condition_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.ConstantConditionError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.ConstantConditionError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("value"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.core.Name("value")))))))), original.value)))))))))

def constant_condition_error_with_value(original: hydra.phantoms.TTerm[hydra.error.core.ConstantConditionError], new_val: hydra.phantoms.TTerm[bool]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.ConstantConditionError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.ConstantConditionError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("value"), new_val.value))))))

def duplicate_binding_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_binding_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_binding_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_binding_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_binding_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateBindingError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def duplicate_field_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_field_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_field_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_field_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_field_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateFieldError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def duplicate_record_type_field_names_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_record_type_field_names_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateRecordTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_record_type_field_names_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateRecordTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_record_type_field_names_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateRecordTypeFieldNamesError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_record_type_field_names_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateRecordTypeFieldNamesError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def duplicate_union_type_field_names_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def duplicate_union_type_field_names_error_location(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateUnionTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.core.Name("location")))))))), x.value))))

def duplicate_union_type_field_names_error_name(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateUnionTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_union_type_field_names_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateUnionTypeFieldNamesError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.core.Name("name")))))))), original.value)))))))))

def duplicate_union_type_field_names_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.DuplicateUnionTypeFieldNamesError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def empty_case_statement_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], type_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("typeName"), type_name.value))))))

def empty_case_statement_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyCaseStatementError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.core.Name("location")))))))), x.value))))

def empty_case_statement_error_type_name(x: hydra.phantoms.TTerm[hydra.error.core.EmptyCaseStatementError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.core.Name("typeName")))))))), x.value))))

def empty_case_statement_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyCaseStatementError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.core.Name("typeName")))))))), original.value)))))))))

def empty_case_statement_error_with_type_name(original: hydra.phantoms.TTerm[hydra.error.core.EmptyCaseStatementError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), new_val.value))))))

def empty_let_bindings_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_let_bindings_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyLetBindingsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), hydra.core.Name("location")))))))), x.value))))

def empty_let_bindings_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyLetBindingsError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def empty_record_type_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_record_type_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyRecordTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), hydra.core.Name("location")))))))), x.value))))

def empty_record_type_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyRecordTypeError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def empty_term_annotation_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_term_annotation_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTermAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), hydra.core.Name("location")))))))), x.value))))

def empty_term_annotation_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyTermAnnotationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def empty_type_annotation_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_type_annotation_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), hydra.core.Name("location")))))))), x.value))))

def empty_type_annotation_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeAnnotationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def empty_type_name_in_term_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_type_name_in_term_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeNameInTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), hydra.core.Name("location")))))))), x.value))))

def empty_type_name_in_term_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeNameInTermError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def empty_union_type_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def empty_union_type_error_location(x: hydra.phantoms.TTerm[hydra.error.core.EmptyUnionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), hydra.core.Name("location")))))))), x.value))))

def empty_union_type_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.EmptyUnionTypeError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def invalid_forall_parameter_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def invalid_forall_parameter_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.InvalidForallParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.core.Name("location")))))))), x.value))))

def invalid_forall_parameter_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidForallParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.core.Name("name")))))))), x.value))))

def invalid_forall_parameter_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.InvalidForallParameterNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def invalid_forall_parameter_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.InvalidForallParameterNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def invalid_lambda_parameter_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def invalid_lambda_parameter_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.core.Name("location")))))))), x.value))))

def invalid_lambda_parameter_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.core.Name("name")))))))), x.value))))

def invalid_lambda_parameter_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.InvalidLambdaParameterNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def invalid_lambda_parameter_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.InvalidLambdaParameterNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def invalid_let_binding_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def invalid_let_binding_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLetBindingNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.core.Name("location")))))))), x.value))))

def invalid_let_binding_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLetBindingNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.core.Name("name")))))))), x.value))))

def invalid_let_binding_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.InvalidLetBindingNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def invalid_let_binding_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.InvalidLetBindingNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def invalid_term_error_constant_condition(x: hydra.phantoms.TTerm[hydra.error.core.ConstantConditionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("constantCondition"), x.value)))))

def invalid_term_error_duplicate_binding(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateBinding"), x.value)))))

def invalid_term_error_duplicate_field(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateField"), x.value)))))

def invalid_term_error_empty_case_statement(x: hydra.phantoms.TTerm[hydra.error.core.EmptyCaseStatementError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyCaseStatement"), x.value)))))

def invalid_term_error_empty_let_bindings(x: hydra.phantoms.TTerm[hydra.error.core.EmptyLetBindingsError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyLetBindings"), x.value)))))

def invalid_term_error_empty_term_annotation(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTermAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyTermAnnotation"), x.value)))))

def invalid_term_error_empty_type_name_in_term(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeNameInTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyTypeNameInTerm"), x.value)))))

def invalid_term_error_invalid_lambda_parameter_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidLambdaParameterName"), x.value)))))

def invalid_term_error_invalid_let_binding_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidLetBindingNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidLetBindingName"), x.value)))))

def invalid_term_error_invalid_type_lambda_parameter_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidTypeLambdaParameterName"), x.value)))))

def invalid_term_error_nested_term_annotation(x: hydra.phantoms.TTerm[hydra.error.core.NestedTermAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("nestedTermAnnotation"), x.value)))))

def invalid_term_error_redundant_wrap_unwrap(x: hydra.phantoms.TTerm[hydra.error.core.RedundantWrapUnwrapError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("redundantWrapUnwrap"), x.value)))))

def invalid_term_error_self_application(x: hydra.phantoms.TTerm[hydra.error.core.SelfApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("selfApplication"), x.value)))))

def invalid_term_error_term_variable_shadowing(x: hydra.phantoms.TTerm[hydra.error.core.TermVariableShadowingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("termVariableShadowing"), x.value)))))

def invalid_term_error_type_variable_shadowing_in_type_lambda(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInTypeLambdaError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("typeVariableShadowingInTypeLambda"), x.value)))))

def invalid_term_error_undefined_term_variable(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), x.value)))))

def invalid_term_error_undefined_type_variable_in_binding_type(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInBindingTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInBindingType"), x.value)))))

def invalid_term_error_undefined_type_variable_in_lambda_domain(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInLambdaDomainError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInLambdaDomain"), x.value)))))

def invalid_term_error_undefined_type_variable_in_type_application(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInTypeApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInTypeApplication"), x.value)))))

def invalid_term_error_unknown_primitive_name(x: hydra.phantoms.TTerm[hydra.error.core.UnknownPrimitiveNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("unknownPrimitiveName"), x.value)))))

def invalid_term_error_unnecessary_identity_application(x: hydra.phantoms.TTerm[hydra.error.core.UnnecessaryIdentityApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("unnecessaryIdentityApplication"), x.value)))))

def invalid_term_error_untyped_term_variable(x: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), x.value)))))

def invalid_type_error_duplicate_record_type_field_names(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateRecordTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("duplicateRecordTypeFieldNames"), x.value)))))

def invalid_type_error_duplicate_union_type_field_names(x: hydra.phantoms.TTerm[hydra.error.core.DuplicateUnionTypeFieldNamesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("duplicateUnionTypeFieldNames"), x.value)))))

def invalid_type_error_empty_record_type(x: hydra.phantoms.TTerm[hydra.error.core.EmptyRecordTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyRecordType"), x.value)))))

def invalid_type_error_empty_type_annotation(x: hydra.phantoms.TTerm[hydra.error.core.EmptyTypeAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyTypeAnnotation"), x.value)))))

def invalid_type_error_empty_union_type(x: hydra.phantoms.TTerm[hydra.error.core.EmptyUnionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyUnionType"), x.value)))))

def invalid_type_error_invalid_forall_parameter_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidForallParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("invalidForallParameterName"), x.value)))))

def invalid_type_error_invalid_type_scheme_variable_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeSchemeVariableNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("invalidTypeSchemeVariableName"), x.value)))))

def invalid_type_error_nested_type_annotation(x: hydra.phantoms.TTerm[hydra.error.core.NestedTypeAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nestedTypeAnnotation"), x.value)))))

def invalid_type_error_non_comparable_map_key_type(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableMapKeyTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nonComparableMapKeyType"), x.value)))))

def invalid_type_error_non_comparable_set_element_type(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableSetElementTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nonComparableSetElementType"), x.value)))))

def invalid_type_error_single_variant_union(x: hydra.phantoms.TTerm[hydra.error.core.SingleVariantUnionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("singleVariantUnion"), x.value)))))

def invalid_type_error_type_variable_shadowing_in_forall(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInForallError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("typeVariableShadowingInForall"), x.value)))))

def invalid_type_error_undefined_type_variable(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariable"), x.value)))))

def invalid_type_error_void_in_non_bottom_position(x: hydra.phantoms.TTerm[hydra.error.core.VoidInNonBottomPositionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("voidInNonBottomPosition"), x.value)))))

def invalid_type_lambda_parameter_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def invalid_type_lambda_parameter_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.core.Name("location")))))))), x.value))))

def invalid_type_lambda_parameter_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeLambdaParameterNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.core.Name("name")))))))), x.value))))

def invalid_type_lambda_parameter_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeLambdaParameterNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def invalid_type_lambda_parameter_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeLambdaParameterNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def invalid_type_scheme_variable_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def invalid_type_scheme_variable_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeSchemeVariableNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.core.Name("location")))))))), x.value))))

def invalid_type_scheme_variable_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeSchemeVariableNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.core.Name("name")))))))), x.value))))

def invalid_type_scheme_variable_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeSchemeVariableNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def invalid_type_scheme_variable_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.InvalidTypeSchemeVariableNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def nested_term_annotation_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def nested_term_annotation_error_location(x: hydra.phantoms.TTerm[hydra.error.core.NestedTermAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), hydra.core.Name("location")))))))), x.value))))

def nested_term_annotation_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.NestedTermAnnotationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def nested_type_annotation_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def nested_type_annotation_error_location(x: hydra.phantoms.TTerm[hydra.error.core.NestedTypeAnnotationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), hydra.core.Name("location")))))))), x.value))))

def nested_type_annotation_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.NestedTypeAnnotationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def non_comparable_map_key_type_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], key_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("keyType"), key_type.value))))))

def non_comparable_map_key_type_error_key_type(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableMapKeyTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.core.Name("keyType")))))))), x.value))))

def non_comparable_map_key_type_error_location(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableMapKeyTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.core.Name("location")))))))), x.value))))

def non_comparable_map_key_type_error_with_key_type(original: hydra.phantoms.TTerm[hydra.error.core.NonComparableMapKeyTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("keyType"), new_val.value))))))

def non_comparable_map_key_type_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.NonComparableMapKeyTypeError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("keyType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), hydra.core.Name("keyType")))))))), original.value)))))))))

def non_comparable_set_element_type_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], element_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("elementType"), element_type.value))))))

def non_comparable_set_element_type_error_element_type(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableSetElementTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.core.Name("elementType")))))))), x.value))))

def non_comparable_set_element_type_error_location(x: hydra.phantoms.TTerm[hydra.error.core.NonComparableSetElementTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.core.Name("location")))))))), x.value))))

def non_comparable_set_element_type_error_with_element_type(original: hydra.phantoms.TTerm[hydra.error.core.NonComparableSetElementTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("elementType"), new_val.value))))))

def non_comparable_set_element_type_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.NonComparableSetElementTypeError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("elementType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), hydra.core.Name("elementType")))))))), original.value)))))))))

def redundant_wrap_unwrap_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], type_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("typeName"), type_name.value))))))

def redundant_wrap_unwrap_error_location(x: hydra.phantoms.TTerm[hydra.error.core.RedundantWrapUnwrapError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.core.Name("location")))))))), x.value))))

def redundant_wrap_unwrap_error_type_name(x: hydra.phantoms.TTerm[hydra.error.core.RedundantWrapUnwrapError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.core.Name("typeName")))))))), x.value))))

def redundant_wrap_unwrap_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.RedundantWrapUnwrapError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.core.Name("typeName")))))))), original.value)))))))))

def redundant_wrap_unwrap_error_with_type_name(original: hydra.phantoms.TTerm[hydra.error.core.RedundantWrapUnwrapError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), new_val.value))))))

def self_application_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SelfApplicationError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def self_application_error_location(x: hydra.phantoms.TTerm[hydra.error.core.SelfApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.core.Name("location")))))))), x.value))))

def self_application_error_name(x: hydra.phantoms.TTerm[hydra.error.core.SelfApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.core.Name("name")))))))), x.value))))

def self_application_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.SelfApplicationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SelfApplicationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.core.Name("name")))))))), original.value)))))))))

def self_application_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.SelfApplicationError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SelfApplicationError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SelfApplicationError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def single_variant_union_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], field_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("fieldName"), field_name.value))))))

def single_variant_union_error_field_name(x: hydra.phantoms.TTerm[hydra.error.core.SingleVariantUnionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.core.Name("fieldName")))))))), x.value))))

def single_variant_union_error_location(x: hydra.phantoms.TTerm[hydra.error.core.SingleVariantUnionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.core.Name("location")))))))), x.value))))

def single_variant_union_error_with_field_name(original: hydra.phantoms.TTerm[hydra.error.core.SingleVariantUnionError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("fieldName"), new_val.value))))))

def single_variant_union_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.SingleVariantUnionError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("fieldName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), hydra.core.Name("fieldName")))))))), original.value)))))))))

def term_variable_shadowing_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def term_variable_shadowing_error_location(x: hydra.phantoms.TTerm[hydra.error.core.TermVariableShadowingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.core.Name("location")))))))), x.value))))

def term_variable_shadowing_error_name(x: hydra.phantoms.TTerm[hydra.error.core.TermVariableShadowingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.core.Name("name")))))))), x.value))))

def term_variable_shadowing_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.TermVariableShadowingError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.core.Name("name")))))))), original.value)))))))))

def term_variable_shadowing_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.TermVariableShadowingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def type_variable_shadowing_in_forall_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def type_variable_shadowing_in_forall_error_location(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInForallError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.core.Name("location")))))))), x.value))))

def type_variable_shadowing_in_forall_error_name(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInForallError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.core.Name("name")))))))), x.value))))

def type_variable_shadowing_in_forall_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInForallError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.core.Name("name")))))))), original.value)))))))))

def type_variable_shadowing_in_forall_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInForallError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def type_variable_shadowing_in_type_lambda_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def type_variable_shadowing_in_type_lambda_error_location(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInTypeLambdaError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.core.Name("location")))))))), x.value))))

def type_variable_shadowing_in_type_lambda_error_name(x: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInTypeLambdaError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.core.Name("name")))))))), x.value))))

def type_variable_shadowing_in_type_lambda_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInTypeLambdaError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.core.Name("name")))))))), original.value)))))))))

def type_variable_shadowing_in_type_lambda_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.TypeVariableShadowingInTypeLambdaError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

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

def undefined_term_variable_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_term_variable_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.core.Name("location")))))))), x.value))))

def undefined_term_variable_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.core.Name("name")))))))), x.value))))

def undefined_term_variable_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.core.Name("name")))))))), original.value)))))))))

def undefined_term_variable_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTermVariableError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def undefined_type_variable_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_type_variable_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.core.Name("location")))))))), x.value))))

def undefined_type_variable_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_variable_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.core.Name("name")))))))), original.value)))))))))

def undefined_type_variable_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def undefined_type_variable_in_binding_type_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_type_variable_in_binding_type_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInBindingTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.core.Name("location")))))))), x.value))))

def undefined_type_variable_in_binding_type_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInBindingTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_variable_in_binding_type_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInBindingTypeError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.core.Name("name")))))))), original.value)))))))))

def undefined_type_variable_in_binding_type_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInBindingTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def undefined_type_variable_in_lambda_domain_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_type_variable_in_lambda_domain_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInLambdaDomainError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.core.Name("location")))))))), x.value))))

def undefined_type_variable_in_lambda_domain_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInLambdaDomainError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_variable_in_lambda_domain_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInLambdaDomainError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.core.Name("name")))))))), original.value)))))))))

def undefined_type_variable_in_lambda_domain_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInLambdaDomainError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def undefined_type_variable_in_type_application_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_type_variable_in_type_application_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInTypeApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.core.Name("location")))))))), x.value))))

def undefined_type_variable_in_type_application_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInTypeApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_variable_in_type_application_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInTypeApplicationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.core.Name("name")))))))), original.value)))))))))

def undefined_type_variable_in_type_application_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UndefinedTypeVariableInTypeApplicationError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

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

def unknown_primitive_name_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def unknown_primitive_name_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UnknownPrimitiveNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.core.Name("location")))))))), x.value))))

def unknown_primitive_name_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UnknownPrimitiveNameError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.core.Name("name")))))))), x.value))))

def unknown_primitive_name_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UnknownPrimitiveNameError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.core.Name("name")))))))), original.value)))))))))

def unknown_primitive_name_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UnknownPrimitiveNameError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def unnecessary_identity_application_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def unnecessary_identity_application_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UnnecessaryIdentityApplicationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), hydra.core.Name("location")))))))), x.value))))

def unnecessary_identity_application_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UnnecessaryIdentityApplicationError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))

def untyped_term_variable_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), location.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def untyped_term_variable_error_location(x: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.core.Name("location")))))))), x.value))))

def untyped_term_variable_error_name(x: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.core.Name("name")))))))), x.value))))

def untyped_term_variable_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.core.Name("name")))))))), original.value)))))))))

def untyped_term_variable_error_with_name(original: hydra.phantoms.TTerm[hydra.error.core.UntypedTermVariableError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), hydra.core.Name("location")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def void_in_non_bottom_position_error(location: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), (hydra.core.Field(hydra.core.Name("location"), location.value),)))))

def void_in_non_bottom_position_error_location(x: hydra.phantoms.TTerm[hydra.error.core.VoidInNonBottomPositionError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), hydra.core.Name("location")))))))), x.value))))

def void_in_non_bottom_position_error_with_location(original: hydra.phantoms.TTerm[hydra.error.core.VoidInNonBottomPositionError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), (hydra.core.Field(hydra.core.Name("location"), new_val.value),)))))
