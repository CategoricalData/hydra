# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def checking_error_incorrect_unification(x: hydra.phantoms.TTerm[hydra.error.IncorrectUnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("incorrectUnification"), x.value)))))

def checking_error_not_a_forall_type(x: hydra.phantoms.TTerm[hydra.error.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("notAForallType"), x.value)))))

def checking_error_not_a_function_type(x: hydra.phantoms.TTerm[hydra.error.NotAFunctionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("notAFunctionType"), x.value)))))

def checking_error_type_arity_mismatch(x: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("typeArityMismatch"), x.value)))))

def checking_error_type_mismatch(x: hydra.phantoms.TTerm[hydra.error.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("typeMismatch"), x.value)))))

def checking_error_unbound_type_variables(x: hydra.phantoms.TTerm[hydra.error.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("unboundTypeVariables"), x.value)))))

def checking_error_unequal_types(x: hydra.phantoms.TTerm[hydra.error.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("unequalTypes"), x.value)))))

def checking_error_unsupported_term_variant(x: hydra.phantoms.TTerm[hydra.error.UnsupportedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("unsupportedTermVariant"), x.value)))))

def checking_error_untyped_lambda(x: hydra.phantoms.TTerm[hydra.error.UntypedLambdaError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLambda"), x.value)))))

def checking_error_untyped_let_binding(x: hydra.phantoms.TTerm[hydra.error.UntypedLetBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLetBinding"), x.value)))))

def decoding_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.error.DecodingError"), x.value))))

def duplicate_binding_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def duplicate_binding_error_name(x: hydra.phantoms.TTerm[hydra.error.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.DuplicateBindingError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_binding_error_with_name(original: hydra.phantoms.TTerm[hydra.error.DuplicateBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def duplicate_field_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def duplicate_field_error_name(x: hydra.phantoms.TTerm[hydra.error.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.DuplicateFieldError"), hydra.core.Name("name")))))))), x.value))))

def duplicate_field_error_with_name(original: hydra.phantoms.TTerm[hydra.error.DuplicateFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def error_checking(x: hydra.phantoms.TTerm[hydra.error.CheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("checking"), x.value)))))

def error_decoding(x: hydra.phantoms.TTerm[hydra.error.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("decoding"), x.value)))))

def error_duplicate_binding(x: hydra.phantoms.TTerm[hydra.error.DuplicateBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("duplicateBinding"), x.value)))))

def error_duplicate_field(x: hydra.phantoms.TTerm[hydra.error.DuplicateFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("duplicateField"), x.value)))))

def error_other(x: hydra.phantoms.TTerm[hydra.error.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def error_undefined_field(x: hydra.phantoms.TTerm[hydra.error.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedField"), x.value)))))

def error_undefined_term(x: hydra.phantoms.TTerm[hydra.error.UndefinedTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedTerm"), x.value)))))

def error_undefined_type(x: hydra.phantoms.TTerm[hydra.error.UndefinedTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("undefinedType"), x.value)))))

def error_unexpected_term_variant(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unexpectedTermVariant"), x.value)))))

def error_unexpected_type_variant(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unexpectedTypeVariant"), x.value)))))

def error_unification(x: hydra.phantoms.TTerm[hydra.error.UnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.Error"), hydra.core.Field(hydra.core.Name("unification"), x.value)))))

def incorrect_unification_error(substitution: hydra.phantoms.TTerm[hydra.typing.TypeSubst]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.IncorrectUnificationError"), (hydra.core.Field(hydra.core.Name("substitution"), substitution.value),)))))

def incorrect_unification_error_substitution(x: hydra.phantoms.TTerm[hydra.error.IncorrectUnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.IncorrectUnificationError"), hydra.core.Name("substitution")))))))), x.value))))

def incorrect_unification_error_with_substitution(original: hydra.phantoms.TTerm[hydra.error.IncorrectUnificationError], new_val: hydra.phantoms.TTerm[hydra.typing.TypeSubst]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.IncorrectUnificationError"), (hydra.core.Field(hydra.core.Name("substitution"), new_val.value),)))))

def not_a_forall_type_error(type: hydra.phantoms.TTerm[hydra.core.Type], type_arguments: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("typeArguments"), type_arguments.value))))))

def not_a_forall_type_error_type(x: hydra.phantoms.TTerm[hydra.error.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.core.Name("type")))))))), x.value))))

def not_a_forall_type_error_type_arguments(x: hydra.phantoms.TTerm[hydra.error.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.core.Name("typeArguments")))))))), x.value))))

def not_a_forall_type_error_with_type(original: hydra.phantoms.TTerm[hydra.error.NotAForallTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def not_a_forall_type_error_with_type_arguments(original: hydra.phantoms.TTerm[hydra.error.NotAForallTypeError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.NotAForallTypeError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), new_val.value))))))

def not_a_function_type_error(type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.NotAFunctionTypeError"), (hydra.core.Field(hydra.core.Name("type"), type.value),)))))

def not_a_function_type_error_type(x: hydra.phantoms.TTerm[hydra.error.NotAFunctionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.NotAFunctionTypeError"), hydra.core.Name("type")))))))), x.value))))

def not_a_function_type_error_with_type(original: hydra.phantoms.TTerm[hydra.error.NotAFunctionTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.NotAFunctionTypeError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value),)))))

def other_error(x: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.error.OtherError"), x.value))))

def type_arity_mismatch_error(type: hydra.phantoms.TTerm[hydra.core.Type], expected_arity: hydra.phantoms.TTerm[int], actual_arity: hydra.phantoms.TTerm[int], type_arguments: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("expectedArity"), expected_arity.value), hydra.core.Field(hydra.core.Name("actualArity"), actual_arity.value), hydra.core.Field(hydra.core.Name("typeArguments"), type_arguments.value))))))

def type_arity_mismatch_error_actual_arity(x: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), x.value))))

def type_arity_mismatch_error_expected_arity(x: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), x.value))))

def type_arity_mismatch_error_type(x: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("type")))))))), x.value))))

def type_arity_mismatch_error_type_arguments(x: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), x.value))))

def type_arity_mismatch_error_with_actual_arity(original: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), new_val.value), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_expected_arity(original: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), new_val.value), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_type(original: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_type_arguments(original: hydra.phantoms.TTerm[hydra.error.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), new_val.value))))))

def type_mismatch_error(expected_type: hydra.phantoms.TTerm[hydra.core.Type], actual_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), expected_type.value), hydra.core.Field(hydra.core.Name("actualType"), actual_type.value))))))

def type_mismatch_error_actual_type(x: hydra.phantoms.TTerm[hydra.error.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeMismatchError"), hydra.core.Name("actualType")))))))), x.value))))

def type_mismatch_error_expected_type(x: hydra.phantoms.TTerm[hydra.error.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeMismatchError"), hydra.core.Name("expectedType")))))))), x.value))))

def type_mismatch_error_with_actual_type(original: hydra.phantoms.TTerm[hydra.error.TypeMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeMismatchError"), hydra.core.Name("expectedType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualType"), new_val.value))))))

def type_mismatch_error_with_expected_type(original: hydra.phantoms.TTerm[hydra.error.TypeMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), new_val.value), hydra.core.Field(hydra.core.Name("actualType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.TypeMismatchError"), hydra.core.Name("actualType")))))))), original.value)))))))))

def un_decoding_error(x: hydra.phantoms.TTerm[hydra.error.DecodingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.error.DecodingError"))))))), x.value))))

def un_other_error(x: hydra.phantoms.TTerm[hydra.error.OtherError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationWrap(hydra.core.Name("hydra.error.OtherError"))))))), x.value))))

def unbound_type_variables_error(variables: hydra.phantoms.TTerm[frozenset[hydra.core.Name]], type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), variables.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def unbound_type_variables_error_type(x: hydra.phantoms.TTerm[hydra.error.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.core.Name("type")))))))), x.value))))

def unbound_type_variables_error_variables(x: hydra.phantoms.TTerm[hydra.error.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.core.Name("variables")))))))), x.value))))

def unbound_type_variables_error_with_type(original: hydra.phantoms.TTerm[hydra.error.UnboundTypeVariablesError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.core.Name("variables")))))))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def unbound_type_variables_error_with_variables(original: hydra.phantoms.TTerm[hydra.error.UnboundTypeVariablesError], new_val: hydra.phantoms.TTerm[frozenset[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnboundTypeVariablesError"), hydra.core.Name("type")))))))), original.value)))))))))

def undefined_field_error(field_name: hydra.phantoms.TTerm[hydra.core.Name], type_name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), field_name.value), hydra.core.Field(hydra.core.Name("typeName"), type_name.value))))))

def undefined_field_error_field_name(x: hydra.phantoms.TTerm[hydra.error.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.core.Name("fieldName")))))))), x.value))))

def undefined_field_error_type_name(x: hydra.phantoms.TTerm[hydra.error.UndefinedFieldError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.core.Name("typeName")))))))), x.value))))

def undefined_field_error_with_field_name(original: hydra.phantoms.TTerm[hydra.error.UndefinedFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), new_val.value), hydra.core.Field(hydra.core.Name("typeName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.core.Name("typeName")))))))), original.value)))))))))

def undefined_field_error_with_type_name(original: hydra.phantoms.TTerm[hydra.error.UndefinedFieldError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedFieldError"), hydra.core.Name("fieldName")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeName"), new_val.value))))))

def undefined_term_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedTermError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def undefined_term_error_name(x: hydra.phantoms.TTerm[hydra.error.UndefinedTermError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedTermError"), hydra.core.Name("name")))))))), x.value))))

def undefined_term_error_with_name(original: hydra.phantoms.TTerm[hydra.error.UndefinedTermError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedTermError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def undefined_type_error(name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedTypeError"), (hydra.core.Field(hydra.core.Name("name"), name.value),)))))

def undefined_type_error_name(x: hydra.phantoms.TTerm[hydra.error.UndefinedTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UndefinedTypeError"), hydra.core.Name("name")))))))), x.value))))

def undefined_type_error_with_name(original: hydra.phantoms.TTerm[hydra.error.UndefinedTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UndefinedTypeError"), (hydra.core.Field(hydra.core.Name("name"), new_val.value),)))))

def unequal_types_error(types: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]], description: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), types.value), hydra.core.Field(hydra.core.Name("description"), description.value))))))

def unequal_types_error_description(x: hydra.phantoms.TTerm[hydra.error.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnequalTypesError"), hydra.core.Name("description")))))))), x.value))))

def unequal_types_error_types(x: hydra.phantoms.TTerm[hydra.error.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnequalTypesError"), hydra.core.Name("types")))))))), x.value))))

def unequal_types_error_with_description(original: hydra.phantoms.TTerm[hydra.error.UnequalTypesError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnequalTypesError"), hydra.core.Name("types")))))))), original.value)))), hydra.core.Field(hydra.core.Name("description"), new_val.value))))))

def unequal_types_error_with_types(original: hydra.phantoms.TTerm[hydra.error.UnequalTypesError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), new_val.value), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnequalTypesError"), hydra.core.Name("description")))))))), original.value)))))))))

def unexpected_term_variant_error(expected_variant: hydra.phantoms.TTerm[hydra.variants.TermVariant], actual_term: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), expected_variant.value), hydra.core.Field(hydra.core.Name("actualTerm"), actual_term.value))))))

def unexpected_term_variant_error_actual_term(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.core.Name("actualTerm")))))))), x.value))))

def unexpected_term_variant_error_expected_variant(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.core.Name("expectedVariant")))))))), x.value))))

def unexpected_term_variant_error_with_actual_term(original: hydra.phantoms.TTerm[hydra.error.UnexpectedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.core.Term]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.core.Name("expectedVariant")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualTerm"), new_val.value))))))

def unexpected_term_variant_error_with_expected_variant(original: hydra.phantoms.TTerm[hydra.error.UnexpectedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), new_val.value), hydra.core.Field(hydra.core.Name("actualTerm"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTermVariantError"), hydra.core.Name("actualTerm")))))))), original.value)))))))))

def unexpected_type_variant_error(expected_variant: hydra.phantoms.TTerm[hydra.variants.TypeVariant], actual_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), expected_variant.value), hydra.core.Field(hydra.core.Name("actualType"), actual_type.value))))))

def unexpected_type_variant_error_actual_type(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.core.Name("actualType")))))))), x.value))))

def unexpected_type_variant_error_expected_variant(x: hydra.phantoms.TTerm[hydra.error.UnexpectedTypeVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.core.Name("expectedVariant")))))))), x.value))))

def unexpected_type_variant_error_with_actual_type(original: hydra.phantoms.TTerm[hydra.error.UnexpectedTypeVariantError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.core.Name("expectedVariant")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualType"), new_val.value))))))

def unexpected_type_variant_error_with_expected_variant(original: hydra.phantoms.TTerm[hydra.error.UnexpectedTypeVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TypeVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), new_val.value), hydra.core.Field(hydra.core.Name("actualType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnexpectedTypeVariantError"), hydra.core.Name("actualType")))))))), original.value)))))))))

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

def unsupported_term_variant_error(term_variant: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnsupportedTermVariantError"), (hydra.core.Field(hydra.core.Name("termVariant"), term_variant.value),)))))

def unsupported_term_variant_error_term_variant(x: hydra.phantoms.TTerm[hydra.error.UnsupportedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UnsupportedTermVariantError"), hydra.core.Name("termVariant")))))))), x.value))))

def unsupported_term_variant_error_with_term_variant(original: hydra.phantoms.TTerm[hydra.error.UnsupportedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UnsupportedTermVariantError"), (hydra.core.Field(hydra.core.Name("termVariant"), new_val.value),)))))

untyped_lambda_error = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UntypedLambdaError"), ()))))

def untyped_let_binding_error(binding: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UntypedLetBindingError"), (hydra.core.Field(hydra.core.Name("binding"), binding.value),)))))

def untyped_let_binding_error_binding(x: hydra.phantoms.TTerm[hydra.error.UntypedLetBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.UntypedLetBindingError"), hydra.core.Name("binding")))))))), x.value))))

def untyped_let_binding_error_with_binding(original: hydra.phantoms.TTerm[hydra.error.UntypedLetBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.UntypedLetBindingError"), (hydra.core.Field(hydra.core.Name("binding"), new_val.value),)))))
