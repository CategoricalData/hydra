# Note: this is an automatically generated file. Do not edit.

r"""DSL functions for hydra.error.checking."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import frozenlist
from typing import cast
import hydra.core
import hydra.phantoms

def checking_error_incorrect_unification(x: hydra.phantoms.TTerm[hydra.error.checking.IncorrectUnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("incorrectUnification"), x.value)))))

def checking_error_not_a_forall_type(x: hydra.phantoms.TTerm[hydra.error.checking.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("notAForallType"), x.value)))))

def checking_error_not_a_function_type(x: hydra.phantoms.TTerm[hydra.error.checking.NotAFunctionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("notAFunctionType"), x.value)))))

def checking_error_other(x: hydra.phantoms.TTerm[hydra.error.checking.OtherCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("other"), x.value)))))

def checking_error_type_arity_mismatch(x: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("typeArityMismatch"), x.value)))))

def checking_error_type_mismatch(x: hydra.phantoms.TTerm[hydra.error.checking.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("typeMismatch"), x.value)))))

def checking_error_unbound_type_variables(x: hydra.phantoms.TTerm[hydra.error.checking.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unboundTypeVariables"), x.value)))))

def checking_error_undefined_term_variable(x: hydra.phantoms.TTerm[hydra.error.checking.UndefinedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), x.value)))))

def checking_error_unequal_types(x: hydra.phantoms.TTerm[hydra.error.checking.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unequalTypes"), x.value)))))

def checking_error_unsupported_term_variant(x: hydra.phantoms.TTerm[hydra.error.checking.UnsupportedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unsupportedTermVariant"), x.value)))))

def checking_error_untyped_lambda(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedLambdaError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLambda"), x.value)))))

def checking_error_untyped_let_binding(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedLetBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLetBinding"), x.value)))))

def checking_error_untyped_term_variable(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), x.value)))))

def incorrect_unification_error(substitution: hydra.phantoms.TTerm[hydra.typing.TypeSubst]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), (hydra.core.Field(hydra.core.Name("substitution"), substitution.value),)))))

def incorrect_unification_error_substitution(x: hydra.phantoms.TTerm[hydra.error.checking.IncorrectUnificationError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), hydra.core.Name("substitution")))))))), x.value))))

def incorrect_unification_error_with_substitution(original: hydra.phantoms.TTerm[hydra.error.checking.IncorrectUnificationError], new_val: hydra.phantoms.TTerm[hydra.typing.TypeSubst]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), (hydra.core.Field(hydra.core.Name("substitution"), new_val.value),)))))

def not_a_forall_type_error(type: hydra.phantoms.TTerm[hydra.core.Type], type_arguments: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("typeArguments"), type_arguments.value))))))

def not_a_forall_type_error_type(x: hydra.phantoms.TTerm[hydra.error.checking.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), hydra.core.Name("type")))))))), x.value))))

def not_a_forall_type_error_type_arguments(x: hydra.phantoms.TTerm[hydra.error.checking.NotAForallTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), hydra.core.Name("typeArguments")))))))), x.value))))

def not_a_forall_type_error_with_type(original: hydra.phantoms.TTerm[hydra.error.checking.NotAForallTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def not_a_forall_type_error_with_type_arguments(original: hydra.phantoms.TTerm[hydra.error.checking.NotAForallTypeError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), new_val.value))))))

def not_a_function_type_error(type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), (hydra.core.Field(hydra.core.Name("type"), type.value),)))))

def not_a_function_type_error_type(x: hydra.phantoms.TTerm[hydra.error.checking.NotAFunctionTypeError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), hydra.core.Name("type")))))))), x.value))))

def not_a_function_type_error_with_type(original: hydra.phantoms.TTerm[hydra.error.checking.NotAFunctionTypeError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value),)))))

def other_checking_error(path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], message: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.OtherCheckingError"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("message"), message.value))))))

def other_checking_error_message(x: hydra.phantoms.TTerm[hydra.error.checking.OtherCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.OtherCheckingError"), hydra.core.Name("message")))))))), x.value))))

def other_checking_error_path(x: hydra.phantoms.TTerm[hydra.error.checking.OtherCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.OtherCheckingError"), hydra.core.Name("path")))))))), x.value))))

def other_checking_error_with_message(original: hydra.phantoms.TTerm[hydra.error.checking.OtherCheckingError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.OtherCheckingError"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.OtherCheckingError"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("message"), new_val.value))))))

def other_checking_error_with_path(original: hydra.phantoms.TTerm[hydra.error.checking.OtherCheckingError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.OtherCheckingError"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.OtherCheckingError"), hydra.core.Name("message")))))))), original.value)))))))))

def type_arity_mismatch_error(type: hydra.phantoms.TTerm[hydra.core.Type], expected_arity: hydra.phantoms.TTerm[int], actual_arity: hydra.phantoms.TTerm[int], type_arguments: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), type.value), hydra.core.Field(hydra.core.Name("expectedArity"), expected_arity.value), hydra.core.Field(hydra.core.Name("actualArity"), actual_arity.value), hydra.core.Field(hydra.core.Name("typeArguments"), type_arguments.value))))))

def type_arity_mismatch_error_actual_arity(x: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), x.value))))

def type_arity_mismatch_error_expected_arity(x: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), x.value))))

def type_arity_mismatch_error_type(x: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("type")))))))), x.value))))

def type_arity_mismatch_error_type_arguments(x: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), x.value))))

def type_arity_mismatch_error_with_actual_arity(original: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), new_val.value), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_expected_arity(original: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[int]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), new_val.value), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_type(original: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), new_val.value), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("typeArguments")))))))), original.value)))))))))

def type_arity_mismatch_error_with_type_arguments(original: hydra.phantoms.TTerm[hydra.error.checking.TypeArityMismatchError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("type")))))))), original.value)))), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("expectedArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), hydra.core.Name("actualArity")))))))), original.value)))), hydra.core.Field(hydra.core.Name("typeArguments"), new_val.value))))))

def type_mismatch_error(expected_type: hydra.phantoms.TTerm[hydra.core.Type], actual_type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), expected_type.value), hydra.core.Field(hydra.core.Name("actualType"), actual_type.value))))))

def type_mismatch_error_actual_type(x: hydra.phantoms.TTerm[hydra.error.checking.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeMismatchError"), hydra.core.Name("actualType")))))))), x.value))))

def type_mismatch_error_expected_type(x: hydra.phantoms.TTerm[hydra.error.checking.TypeMismatchError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeMismatchError"), hydra.core.Name("expectedType")))))))), x.value))))

def type_mismatch_error_with_actual_type(original: hydra.phantoms.TTerm[hydra.error.checking.TypeMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeMismatchError"), hydra.core.Name("expectedType")))))))), original.value)))), hydra.core.Field(hydra.core.Name("actualType"), new_val.value))))))

def type_mismatch_error_with_expected_type(original: hydra.phantoms.TTerm[hydra.error.checking.TypeMismatchError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), new_val.value), hydra.core.Field(hydra.core.Name("actualType"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.TypeMismatchError"), hydra.core.Name("actualType")))))))), original.value)))))))))

def unbound_type_variables_error(variables: hydra.phantoms.TTerm[frozenset[hydra.core.Name]], type: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), variables.value), hydra.core.Field(hydra.core.Name("type"), type.value))))))

def unbound_type_variables_error_type(x: hydra.phantoms.TTerm[hydra.error.checking.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), hydra.core.Name("type")))))))), x.value))))

def unbound_type_variables_error_variables(x: hydra.phantoms.TTerm[hydra.error.checking.UnboundTypeVariablesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), hydra.core.Name("variables")))))))), x.value))))

def unbound_type_variables_error_with_type(original: hydra.phantoms.TTerm[hydra.error.checking.UnboundTypeVariablesError], new_val: hydra.phantoms.TTerm[hydra.core.Type]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), hydra.core.Name("variables")))))))), original.value)))), hydra.core.Field(hydra.core.Name("type"), new_val.value))))))

def unbound_type_variables_error_with_variables(original: hydra.phantoms.TTerm[hydra.error.checking.UnboundTypeVariablesError], new_val: hydra.phantoms.TTerm[frozenset[hydra.core.Name]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), new_val.value), hydra.core.Field(hydra.core.Name("type"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), hydra.core.Name("type")))))))), original.value)))))))))

def undefined_term_variable_checking_error(path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def undefined_term_variable_checking_error_name(x: hydra.phantoms.TTerm[hydra.error.checking.UndefinedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), hydra.core.Name("name")))))))), x.value))))

def undefined_term_variable_checking_error_path(x: hydra.phantoms.TTerm[hydra.error.checking.UndefinedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), hydra.core.Name("path")))))))), x.value))))

def undefined_term_variable_checking_error_with_name(original: hydra.phantoms.TTerm[hydra.error.checking.UndefinedTermVariableCheckingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def undefined_term_variable_checking_error_with_path(original: hydra.phantoms.TTerm[hydra.error.checking.UndefinedTermVariableCheckingError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), hydra.core.Name("name")))))))), original.value)))))))))

def unequal_types_error(types: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]], description: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), types.value), hydra.core.Field(hydra.core.Name("description"), description.value))))))

def unequal_types_error_description(x: hydra.phantoms.TTerm[hydra.error.checking.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnequalTypesError"), hydra.core.Name("description")))))))), x.value))))

def unequal_types_error_types(x: hydra.phantoms.TTerm[hydra.error.checking.UnequalTypesError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnequalTypesError"), hydra.core.Name("types")))))))), x.value))))

def unequal_types_error_with_description(original: hydra.phantoms.TTerm[hydra.error.checking.UnequalTypesError], new_val: hydra.phantoms.TTerm[str]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnequalTypesError"), hydra.core.Name("types")))))))), original.value)))), hydra.core.Field(hydra.core.Name("description"), new_val.value))))))

def unequal_types_error_with_types(original: hydra.phantoms.TTerm[hydra.error.checking.UnequalTypesError], new_val: hydra.phantoms.TTerm[frozenlist[hydra.core.Type]]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), new_val.value), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnequalTypesError"), hydra.core.Name("description")))))))), original.value)))))))))

def unsupported_term_variant_error(term_variant: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), (hydra.core.Field(hydra.core.Name("termVariant"), term_variant.value),)))))

def unsupported_term_variant_error_term_variant(x: hydra.phantoms.TTerm[hydra.error.checking.UnsupportedTermVariantError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), hydra.core.Name("termVariant")))))))), x.value))))

def unsupported_term_variant_error_with_term_variant(original: hydra.phantoms.TTerm[hydra.error.checking.UnsupportedTermVariantError], new_val: hydra.phantoms.TTerm[hydra.variants.TermVariant]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), (hydra.core.Field(hydra.core.Name("termVariant"), new_val.value),)))))

untyped_lambda_error = hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedLambdaError"), ()))))

def untyped_let_binding_error(binding: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), (hydra.core.Field(hydra.core.Name("binding"), binding.value),)))))

def untyped_let_binding_error_binding(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedLetBindingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), hydra.core.Name("binding")))))))), x.value))))

def untyped_let_binding_error_with_binding(original: hydra.phantoms.TTerm[hydra.error.checking.UntypedLetBindingError], new_val: hydra.phantoms.TTerm[hydra.core.Binding]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), (hydra.core.Field(hydra.core.Name("binding"), new_val.value),)))))

def untyped_term_variable_checking_error(path: hydra.phantoms.TTerm[hydra.paths.SubtermPath], name: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), path.value), hydra.core.Field(hydra.core.Name("name"), name.value))))))

def untyped_term_variable_checking_error_name(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), hydra.core.Name("name")))))))), x.value))))

def untyped_term_variable_checking_error_path(x: hydra.phantoms.TTerm[hydra.error.checking.UntypedTermVariableCheckingError]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), hydra.core.Name("path")))))))), x.value))))

def untyped_term_variable_checking_error_with_name(original: hydra.phantoms.TTerm[hydra.error.checking.UntypedTermVariableCheckingError], new_val: hydra.phantoms.TTerm[hydra.core.Name]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), hydra.core.Name("path")))))))), original.value)))), hydra.core.Field(hydra.core.Name("name"), new_val.value))))))

def untyped_term_variable_checking_error_with_path(original: hydra.phantoms.TTerm[hydra.error.checking.UntypedTermVariableCheckingError], new_val: hydra.phantoms.TTerm[hydra.paths.SubtermPath]) -> hydra.phantoms.TTerm:
    return hydra.phantoms.TTerm(cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), new_val.value), hydra.core.Field(hydra.core.Name("name"), cast(hydra.core.Term, hydra.core.TermApplication(hydra.core.Application(cast(hydra.core.Term, hydra.core.TermFunction(cast(hydra.core.Function, hydra.core.FunctionElimination(cast(hydra.core.Elimination, hydra.core.EliminationRecord(hydra.core.Projection(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), hydra.core.Name("name")))))))), original.value)))))))))
