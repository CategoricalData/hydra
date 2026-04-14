# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.error.checking."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.encode.paths
import hydra.encode.typing
import hydra.encode.variants
import hydra.error.checking
import hydra.lib.lists
import hydra.lib.sets

T0 = TypeVar("T0")

def incorrect_unification_error(x: hydra.error.checking.IncorrectUnificationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.IncorrectUnificationError"), (hydra.core.Field(hydra.core.Name("substitution"), hydra.encode.typing.type_subst(x.substitution)),))))

def not_a_forall_type_error(x: hydra.error.checking.NotAForallTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAForallTypeError"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.type(x1)), x.type_arguments))))))))

def not_a_function_type_error(x: hydra.error.checking.NotAFunctionTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.NotAFunctionTypeError"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)),))))

def other_checking_error(x: hydra.error.checking.OtherCheckingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.OtherCheckingError"), (hydra.core.Field(hydra.core.Name("path"), hydra.encode.paths.subterm_path(x.path)), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.message)))))))))

def type_arity_mismatch_error(x: hydra.error.checking.TypeArityMismatchError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeArityMismatchError"), (hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type)), hydra.core.Field(hydra.core.Name("expectedArity"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x.expected_arity))))))), hydra.core.Field(hydra.core.Name("actualArity"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralInteger(cast(hydra.core.IntegerValue, hydra.core.IntegerValueInt32(x.actual_arity))))))), hydra.core.Field(hydra.core.Name("typeArguments"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.type(x1)), x.type_arguments))))))))

def type_mismatch_error(x: hydra.error.checking.TypeMismatchError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.TypeMismatchError"), (hydra.core.Field(hydra.core.Name("expectedType"), hydra.encode.core.type(x.expected_type)), hydra.core.Field(hydra.core.Name("actualType"), hydra.encode.core.type(x.actual_type))))))

def unbound_type_variables_error(x: hydra.error.checking.UnboundTypeVariablesError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError"), (hydra.core.Field(hydra.core.Name("variables"), cast(hydra.core.Term, hydra.core.TermSet(hydra.lib.sets.map((lambda x1: hydra.encode.core.name(x1)), x.variables)))), hydra.core.Field(hydra.core.Name("type"), hydra.encode.core.type(x.type))))))

def undefined_term_variable_checking_error(x: hydra.error.checking.UndefinedTermVariableCheckingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), hydra.encode.paths.subterm_path(x.path)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def unequal_types_error(x: hydra.error.checking.UnequalTypesError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnequalTypesError"), (hydra.core.Field(hydra.core.Name("types"), cast(hydra.core.Term, hydra.core.TermList(hydra.lib.lists.map((lambda x1: hydra.encode.core.type(x1)), x.types)))), hydra.core.Field(hydra.core.Name("description"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.description)))))))))

def unsupported_term_variant_error(x: hydra.error.checking.UnsupportedTermVariantError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError"), (hydra.core.Field(hydra.core.Name("termVariant"), hydra.encode.variants.term_variant(x.term_variant)),))))

def untyped_lambda_error(x: T0) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedLambdaError"), ())))

def untyped_let_binding_error(x: hydra.error.checking.UntypedLetBindingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedLetBindingError"), (hydra.core.Field(hydra.core.Name("binding"), hydra.encode.core.binding(x.binding)),))))

def untyped_term_variable_checking_error(x: hydra.error.checking.UntypedTermVariableCheckingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError"), (hydra.core.Field(hydra.core.Name("path"), hydra.encode.paths.subterm_path(x.path)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def checking_error(v1: hydra.error.checking.CheckingError) -> hydra.core.Term:
    match v1:
        case hydra.error.checking.CheckingErrorIncorrectUnification(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("incorrectUnification"), incorrect_unification_error(y)))))

        case hydra.error.checking.CheckingErrorNotAForallType(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("notAForallType"), not_a_forall_type_error(y2)))))

        case hydra.error.checking.CheckingErrorNotAFunctionType(value=y3):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("notAFunctionType"), not_a_function_type_error(y3)))))

        case hydra.error.checking.CheckingErrorOther(value=y4):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("other"), other_checking_error(y4)))))

        case hydra.error.checking.CheckingErrorTypeArityMismatch(value=y5):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("typeArityMismatch"), type_arity_mismatch_error(y5)))))

        case hydra.error.checking.CheckingErrorTypeMismatch(value=y6):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("typeMismatch"), type_mismatch_error(y6)))))

        case hydra.error.checking.CheckingErrorUnboundTypeVariables(value=y7):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unboundTypeVariables"), unbound_type_variables_error(y7)))))

        case hydra.error.checking.CheckingErrorUndefinedTermVariable(value=y8):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), undefined_term_variable_checking_error(y8)))))

        case hydra.error.checking.CheckingErrorUnequalTypes(value=y9):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unequalTypes"), unequal_types_error(y9)))))

        case hydra.error.checking.CheckingErrorUnsupportedTermVariant(value=y10):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("unsupportedTermVariant"), unsupported_term_variant_error(y10)))))

        case hydra.error.checking.CheckingErrorUntypedLambda(value=y11):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLambda"), untyped_lambda_error(y11)))))

        case hydra.error.checking.CheckingErrorUntypedLetBinding(value=y12):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedLetBinding"), untyped_let_binding_error(y12)))))

        case hydra.error.checking.CheckingErrorUntypedTermVariable(value=y13):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.error.checking.CheckingError"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), untyped_term_variable_checking_error(y13)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")
