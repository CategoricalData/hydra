# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.error.core types."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.error.core
import hydra.lib.literals
import hydra.lib.strings
import hydra.show.core
import hydra.show.variants

T0 = TypeVar("T0")

def constant_condition_error(e: hydra.error.core.ConstantConditionError) -> str:
    r"""Show a constant condition error as a string."""

    return hydra.lib.strings.cat(("constant condition: ifElse with literal ", hydra.lib.literals.show_boolean(e.value)))

def duplicate_binding_error(e: hydra.error.core.DuplicateBindingError) -> str:
    r"""Show a duplicate binding error as a string."""

    return hydra.lib.strings.cat(("duplicate binding: ", e.name.value))

def duplicate_field_error(e: hydra.error.core.DuplicateFieldError) -> str:
    r"""Show a duplicate field error as a string."""

    return hydra.lib.strings.cat(("duplicate field: ", e.name.value))

def duplicate_record_type_field_names_error(e: hydra.error.core.DuplicateRecordTypeFieldNamesError) -> str:
    r"""Show a duplicate record type field names error as a string."""

    return hydra.lib.strings.cat(("duplicate field in record type: ", e.name.value))

def duplicate_union_type_field_names_error(e: hydra.error.core.DuplicateUnionTypeFieldNamesError) -> str:
    r"""Show a duplicate union type field names error as a string."""

    return hydra.lib.strings.cat(("duplicate field in union type: ", e.name.value))

def empty_case_statement_error(e: hydra.error.core.EmptyCaseStatementError) -> str:
    r"""Show an empty case statement error as a string."""

    return hydra.lib.strings.cat(("empty case statement for type: ", e.type_name.value))

def empty_let_bindings_error(e: T0) -> str:
    r"""Show an empty let bindings error as a string."""

    return "let expression with no bindings"

def empty_record_type_error(e: T0) -> str:
    r"""Show an empty record type error as a string."""

    return "record type with no fields (use TypeUnit instead)"

def empty_term_annotation_error(e: T0) -> str:
    r"""Show an empty term annotation error as a string."""

    return "term annotation with empty annotation map"

def empty_type_annotation_error(e: T0) -> str:
    r"""Show an empty type annotation error as a string."""

    return "type annotation with empty annotation map"

def empty_type_name_in_term_error(e: T0) -> str:
    r"""Show an empty type name in term error as a string."""

    return "term with empty type name"

def empty_union_type_error(e: T0) -> str:
    r"""Show an empty union type error as a string."""

    return "union type with no alternatives (use TypeVoid instead)"

def invalid_forall_parameter_name_error(e: hydra.error.core.InvalidForallParameterNameError) -> str:
    r"""Show an invalid forall parameter name error as a string."""

    return hydra.lib.strings.cat(("invalid forall parameter name: ", e.name.value))

def invalid_lambda_parameter_name_error(e: hydra.error.core.InvalidLambdaParameterNameError) -> str:
    r"""Show an invalid lambda parameter name error as a string."""

    return hydra.lib.strings.cat(("invalid lambda parameter name: ", e.name.value))

def invalid_let_binding_name_error(e: hydra.error.core.InvalidLetBindingNameError) -> str:
    r"""Show an invalid let binding name error as a string."""

    return hydra.lib.strings.cat(("invalid let binding name: ", e.name.value))

def invalid_type_lambda_parameter_name_error(e: hydra.error.core.InvalidTypeLambdaParameterNameError) -> str:
    r"""Show an invalid type lambda parameter name error as a string."""

    return hydra.lib.strings.cat(("invalid type lambda parameter name: ", e.name.value))

def nested_term_annotation_error(e: T0) -> str:
    r"""Show a nested term annotation error as a string."""

    return "nested term annotations should be merged"

def redundant_wrap_unwrap_error(e: hydra.error.core.RedundantWrapUnwrapError) -> str:
    r"""Show a redundant wrap/unwrap error as a string."""

    return hydra.lib.strings.cat(("redundant wrap/unwrap for type: ", e.type_name.value))

def self_application_error(e: hydra.error.core.SelfApplicationError) -> str:
    r"""Show a self-application error as a string."""

    return hydra.lib.strings.cat(("self-application of variable: ", e.name.value))

def term_variable_shadowing_error(e: hydra.error.core.TermVariableShadowingError) -> str:
    r"""Show a term variable shadowing error as a string."""

    return hydra.lib.strings.cat(("variable shadowing: ", e.name.value))

def type_variable_shadowing_in_type_lambda_error(e: hydra.error.core.TypeVariableShadowingInTypeLambdaError) -> str:
    r"""Show a type variable shadowing in type lambda error as a string."""

    return hydra.lib.strings.cat(("type variable shadowing in type lambda: ", e.name.value))

def undefined_term_variable_error(e: hydra.error.core.UndefinedTermVariableError) -> str:
    r"""Show an undefined term variable error as a string."""

    return hydra.lib.strings.cat(("undefined term variable: ", e.name.value))

def undefined_type_variable_in_binding_type_error(e: hydra.error.core.UndefinedTypeVariableInBindingTypeError) -> str:
    r"""Show an undefined type variable in binding type error as a string."""

    return hydra.lib.strings.cat(("undefined type variable in binding type: ", e.name.value))

def undefined_type_variable_in_lambda_domain_error(e: hydra.error.core.UndefinedTypeVariableInLambdaDomainError) -> str:
    r"""Show an undefined type variable in lambda domain error as a string."""

    return hydra.lib.strings.cat(("undefined type variable in lambda domain: ", e.name.value))

def undefined_type_variable_in_type_application_error(e: hydra.error.core.UndefinedTypeVariableInTypeApplicationError) -> str:
    r"""Show an undefined type variable in type application error as a string."""

    return hydra.lib.strings.cat(("undefined type variable in type application: ", e.name.value))

def unknown_primitive_name_error(e: hydra.error.core.UnknownPrimitiveNameError) -> str:
    r"""Show an unknown primitive name error as a string."""

    return hydra.lib.strings.cat(("unknown primitive: ", e.name.value))

def unnecessary_identity_application_error(e: T0) -> str:
    r"""Show an unnecessary identity application error as a string."""

    return "unnecessary application of identity lambda"

def untyped_term_variable_error(e: hydra.error.core.UntypedTermVariableError) -> str:
    r"""Show an untyped term variable error as a string."""

    return hydra.lib.strings.cat(("untyped term variable: ", e.name.value))

def invalid_term_error(e: hydra.error.core.InvalidTermError):
    def _hoist_hydra_show_error_core_invalid_term_error_1(v1):
        match v1:
            case hydra.error.core.InvalidTermErrorConstantCondition(value=v12):
                return constant_condition_error(v12)

            case hydra.error.core.InvalidTermErrorDuplicateBinding(value=v12):
                return duplicate_binding_error(v12)

            case hydra.error.core.InvalidTermErrorDuplicateField(value=v12):
                return duplicate_field_error(v12)

            case hydra.error.core.InvalidTermErrorEmptyCaseStatement(value=v12):
                return empty_case_statement_error(v12)

            case hydra.error.core.InvalidTermErrorEmptyLetBindings(value=v12):
                return empty_let_bindings_error(v12)

            case hydra.error.core.InvalidTermErrorEmptyTermAnnotation(value=v12):
                return empty_term_annotation_error(v12)

            case hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(value=v12):
                return empty_type_name_in_term_error(v12)

            case hydra.error.core.InvalidTermErrorInvalidLambdaParameterName(value=v12):
                return invalid_lambda_parameter_name_error(v12)

            case hydra.error.core.InvalidTermErrorInvalidLetBindingName(value=v12):
                return invalid_let_binding_name_error(v12)

            case hydra.error.core.InvalidTermErrorInvalidTypeLambdaParameterName(value=v12):
                return invalid_type_lambda_parameter_name_error(v12)

            case hydra.error.core.InvalidTermErrorNestedTermAnnotation(value=v12):
                return nested_term_annotation_error(v12)

            case hydra.error.core.InvalidTermErrorRedundantWrapUnwrap(value=v12):
                return redundant_wrap_unwrap_error(v12)

            case hydra.error.core.InvalidTermErrorSelfApplication(value=v12):
                return self_application_error(v12)

            case hydra.error.core.InvalidTermErrorTermVariableShadowing(value=v12):
                return term_variable_shadowing_error(v12)

            case hydra.error.core.InvalidTermErrorTypeVariableShadowingInTypeLambda(value=v12):
                return type_variable_shadowing_in_type_lambda_error(v12)

            case hydra.error.core.InvalidTermErrorUndefinedTermVariable(value=v12):
                return undefined_term_variable_error(v12)

            case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInBindingType(value=v12):
                return undefined_type_variable_in_binding_type_error(v12)

            case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInLambdaDomain(value=v12):
                return undefined_type_variable_in_lambda_domain_error(v12)

            case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInTypeApplication(value=v12):
                return undefined_type_variable_in_type_application_error(v12)

            case hydra.error.core.InvalidTermErrorUnknownPrimitiveName(value=v12):
                return unknown_primitive_name_error(v12)

            case hydra.error.core.InvalidTermErrorUnnecessaryIdentityApplication(value=v12):
                return unnecessary_identity_application_error(v12)

            case hydra.error.core.InvalidTermErrorUntypedTermVariable(value=v12):
                return untyped_term_variable_error(v12)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.strings.cat2("invalid term: ", _hoist_hydra_show_error_core_invalid_term_error_1(e))

def invalid_type_scheme_variable_name_error(e: hydra.error.core.InvalidTypeSchemeVariableNameError) -> str:
    r"""Show an invalid type scheme variable name error as a string."""

    return hydra.lib.strings.cat(("invalid type scheme variable name: ", e.name.value))

def nested_type_annotation_error(e: T0) -> str:
    r"""Show a nested type annotation error as a string."""

    return "nested type annotations should be merged"

def non_comparable_map_key_type_error(e: hydra.error.core.NonComparableMapKeyTypeError) -> str:
    r"""Show a non-comparable map key type error as a string."""

    return hydra.lib.strings.cat(("map key type contains a function type: ", hydra.show.core.type(e.key_type)))

def non_comparable_set_element_type_error(e: hydra.error.core.NonComparableSetElementTypeError) -> str:
    r"""Show a non-comparable set element type error as a string."""

    return hydra.lib.strings.cat(("set element type contains a function type: ", hydra.show.core.type(e.element_type)))

def single_variant_union_error(e: hydra.error.core.SingleVariantUnionError) -> str:
    r"""Show a single variant union error as a string."""

    return hydra.lib.strings.cat(("union type with single variant: ", e.field_name.value))

def type_variable_shadowing_in_forall_error(e: hydra.error.core.TypeVariableShadowingInForallError) -> str:
    r"""Show a type variable shadowing in forall error as a string."""

    return hydra.lib.strings.cat(("type variable shadowing in forall: ", e.name.value))

def undefined_type_variable_error(e: hydra.error.core.UndefinedTypeVariableError) -> str:
    r"""Show an undefined type variable error as a string."""

    return hydra.lib.strings.cat(("undefined type variable: ", e.name.value))

def void_in_non_bottom_position_error(e: T0) -> str:
    r"""Show a void in non-bottom position error as a string."""

    return "TypeVoid in a position where no value can be constructed"

def invalid_type_error(e: hydra.error.core.InvalidTypeError):
    def _hoist_hydra_show_error_core_invalid_type_error_1(v1):
        match v1:
            case hydra.error.core.InvalidTypeErrorDuplicateRecordTypeFieldNames(value=v12):
                return duplicate_record_type_field_names_error(v12)

            case hydra.error.core.InvalidTypeErrorDuplicateUnionTypeFieldNames(value=v12):
                return duplicate_union_type_field_names_error(v12)

            case hydra.error.core.InvalidTypeErrorEmptyRecordType(value=v12):
                return empty_record_type_error(v12)

            case hydra.error.core.InvalidTypeErrorEmptyTypeAnnotation(value=v12):
                return empty_type_annotation_error(v12)

            case hydra.error.core.InvalidTypeErrorEmptyUnionType(value=v12):
                return empty_union_type_error(v12)

            case hydra.error.core.InvalidTypeErrorInvalidForallParameterName(value=v12):
                return invalid_forall_parameter_name_error(v12)

            case hydra.error.core.InvalidTypeErrorInvalidTypeSchemeVariableName(value=v12):
                return invalid_type_scheme_variable_name_error(v12)

            case hydra.error.core.InvalidTypeErrorNestedTypeAnnotation(value=v12):
                return nested_type_annotation_error(v12)

            case hydra.error.core.InvalidTypeErrorNonComparableMapKeyType(value=v12):
                return non_comparable_map_key_type_error(v12)

            case hydra.error.core.InvalidTypeErrorNonComparableSetElementType(value=v12):
                return non_comparable_set_element_type_error(v12)

            case hydra.error.core.InvalidTypeErrorSingleVariantUnion(value=v12):
                return single_variant_union_error(v12)

            case hydra.error.core.InvalidTypeErrorTypeVariableShadowingInForall(value=v12):
                return type_variable_shadowing_in_forall_error(v12)

            case hydra.error.core.InvalidTypeErrorUndefinedTypeVariable(value=v12):
                return undefined_type_variable_error(v12)

            case hydra.error.core.InvalidTypeErrorVoidInNonBottomPosition(value=v12):
                return void_in_non_bottom_position_error(v12)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.strings.cat2("invalid type: ", _hoist_hydra_show_error_core_invalid_type_error_1(e))

def undefined_field_error(e: hydra.error.core.UndefinedFieldError) -> str:
    r"""Show an undefined field error as a string."""

    fname = e.field_name
    tname = e.type_name
    return hydra.lib.strings.cat(("no such field \"", fname.value, "\" in type \"", tname.value, "\""))

def unexpected_term_variant_error(e: hydra.error.core.UnexpectedTermVariantError) -> str:
    r"""Show an unexpected term variant error as a string."""

    expected = e.expected_variant
    actual = e.actual_term
    return hydra.lib.strings.cat(("expected ", hydra.show.variants.term_variant(expected), " term but found ", hydra.show.core.term(actual)))

def unexpected_type_variant_error(e: hydra.error.core.UnexpectedTypeVariantError) -> str:
    r"""Show an unexpected type variant error as a string."""

    expected = e.expected_variant
    actual = e.actual_type
    return hydra.lib.strings.cat(("expected ", hydra.show.variants.type_variant(expected), " type but found ", hydra.show.core.type(actual)))
