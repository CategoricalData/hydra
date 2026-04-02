# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.error types."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.error.checking
import hydra.errors
import hydra.formatting
import hydra.lib.lists
import hydra.lib.literals
import hydra.lib.sets
import hydra.lib.strings
import hydra.show.core
import hydra.show.error.core
import hydra.show.typing
import hydra.show.variants

T0 = TypeVar("T0")

def incorrect_unification_error(e: hydra.error.checking.IncorrectUnificationError) -> str:
    r"""Show an incorrect unification error as a string."""

    subst = e.substitution
    return hydra.lib.strings.cat2("incorrect unification: ", hydra.show.typing.type_subst(subst))

def not_a_forall_type_error(e: hydra.error.checking.NotAForallTypeError) -> str:
    r"""Show a not-a-forall-type error as a string."""

    typ = e.type
    args = e.type_arguments
    return hydra.lib.strings.cat(("not a forall type: ", hydra.show.core.type(typ), ". Trying to apply ", hydra.lib.literals.show_int32(hydra.lib.lists.length(args)), " type argument(s): ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), args)))

def not_a_function_type_error(e: hydra.error.checking.NotAFunctionTypeError) -> str:
    r"""Show a not-a-function-type error as a string."""

    typ = e.type
    return hydra.lib.strings.cat2("not a function type: ", hydra.show.core.type(typ))

def type_arity_mismatch_error(e: hydra.error.checking.TypeArityMismatchError) -> str:
    r"""Show a type arity mismatch error as a string."""

    typ = e.type
    expected = e.expected_arity
    actual = e.actual_arity
    args = e.type_arguments
    return hydra.lib.strings.cat(("type ", hydra.show.core.type(typ), " applied to the wrong number of type arguments (expected ", hydra.lib.literals.show_int32(expected), ", got ", hydra.lib.literals.show_int32(actual), "): ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), args)))

def type_mismatch_error(e: hydra.error.checking.TypeMismatchError) -> str:
    r"""Show a type mismatch error as a string."""

    expected = e.expected_type
    actual = e.actual_type
    return hydra.lib.strings.cat(("type mismatch: expected ", hydra.show.core.type(expected), " but found ", hydra.show.core.type(actual)))

def unbound_type_variables_error(e: hydra.error.checking.UnboundTypeVariablesError) -> str:
    r"""Show an unbound type variables error as a string."""

    vars = e.variables
    typ = e.type
    return hydra.lib.strings.cat(("unbound type variables: {", hydra.lib.strings.intercalate(", ", hydra.lib.lists.map((lambda v1: v1.value), hydra.lib.sets.to_list(vars))), "} in type ", hydra.show.core.type(typ)))

def unequal_types_error(e: hydra.error.checking.UnequalTypesError) -> str:
    r"""Show an unequal types error as a string."""

    types = e.types
    desc = e.description
    return hydra.lib.strings.cat(("unequal types ", hydra.formatting.show_list((lambda x1: hydra.show.core.type(x1)), types), " in ", desc))

def unsupported_term_variant_error(e: hydra.error.checking.UnsupportedTermVariantError) -> str:
    r"""Show an unsupported term variant error as a string."""

    return hydra.lib.strings.cat2("unsupported term variant: ", hydra.show.variants.term_variant(e.term_variant))

def untyped_lambda_error(_: T0) -> str:
    r"""Show an untyped lambda error as a string."""

    return "untyped lambda"

def untyped_let_binding_error(e: hydra.error.checking.UntypedLetBindingError) -> str:
    r"""Show an untyped let binding error as a string."""

    b = e.binding
    return hydra.lib.strings.cat2("untyped let binding: ", hydra.show.core.binding(b))

def checking_error(ce: hydra.error.checking.CheckingError) -> str:
    r"""Show a checking error as a string."""

    match ce:
        case hydra.error.checking.CheckingErrorIncorrectUnification(value=v1):
            return incorrect_unification_error(v1)

        case hydra.error.checking.CheckingErrorNotAForallType(value=v12):
            return not_a_forall_type_error(v12)

        case hydra.error.checking.CheckingErrorNotAFunctionType(value=v13):
            return not_a_function_type_error(v13)

        case hydra.error.checking.CheckingErrorTypeArityMismatch(value=v14):
            return type_arity_mismatch_error(v14)

        case hydra.error.checking.CheckingErrorTypeMismatch(value=v15):
            return type_mismatch_error(v15)

        case hydra.error.checking.CheckingErrorUnboundTypeVariables(value=v16):
            return unbound_type_variables_error(v16)

        case hydra.error.checking.CheckingErrorUnequalTypes(value=v17):
            return unequal_types_error(v17)

        case hydra.error.checking.CheckingErrorUnsupportedTermVariant(value=v18):
            return unsupported_term_variant_error(v18)

        case hydra.error.checking.CheckingErrorUntypedLambda(value=v19):
            return untyped_lambda_error(v19)

        case hydra.error.checking.CheckingErrorUntypedLetBinding(value=v110):
            return untyped_let_binding_error(v110)

        case _:
            raise AssertionError("Unreachable: all variants handled")

def decoding_error(de: hydra.errors.DecodingError) -> str:
    r"""Show a decoding error as a string."""

    return hydra.lib.strings.cat2("decoding error: ", de.value)

def other_error(oe: hydra.errors.OtherError) -> str:
    r"""Show an other error as a string."""

    return oe.value

def unification_error(e: hydra.errors.UnificationError) -> str:
    r"""Show a unification error as a string."""

    lt = e.left_type
    rt = e.right_type
    msg = e.message
    return hydra.lib.strings.cat(("unification error: cannot unify ", hydra.show.core.type(lt), " with ", hydra.show.core.type(rt), ": ", msg))

def error(e: hydra.errors.Error) -> str:
    r"""Show an error as a string."""

    match e:
        case hydra.errors.ErrorChecking(value=v1):
            return checking_error(v1)

        case hydra.errors.ErrorDecoding(value=v12):
            return decoding_error(v12)

        case hydra.errors.ErrorDuplicateBinding(value=v13):
            return hydra.show.error.core.duplicate_binding_error(v13)

        case hydra.errors.ErrorDuplicateField(value=v14):
            return hydra.show.error.core.duplicate_field_error(v14)

        case hydra.errors.ErrorOther(value=v15):
            return other_error(v15)

        case hydra.errors.ErrorUndefinedField(value=v16):
            return hydra.show.error.core.undefined_field_error(v16)

        case hydra.errors.ErrorUndefinedTermVariable(value=v17):
            return hydra.show.error.core.undefined_term_variable_error(v17)

        case hydra.errors.ErrorUntypedTermVariable(value=v18):
            return hydra.show.error.core.untyped_term_variable_error(v18)

        case hydra.errors.ErrorUnexpectedTermVariant(value=v19):
            return hydra.show.error.core.unexpected_term_variant_error(v19)

        case hydra.errors.ErrorUnexpectedTypeVariant(value=v110):
            return hydra.show.error.core.unexpected_type_variant_error(v110)

        case hydra.errors.ErrorUnification(value=v111):
            return unification_error(v111)

        case _:
            raise AssertionError("Unreachable: all variants handled")
