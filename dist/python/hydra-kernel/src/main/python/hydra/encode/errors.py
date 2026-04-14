# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.errors."""

from __future__ import annotations
from functools import lru_cache
from typing import TypeVar, cast
import hydra.core
import hydra.encode.core
import hydra.encode.error.checking
import hydra.encode.error.core
import hydra.encode.paths
import hydra.errors

T0 = TypeVar("T0")

def decoding_error(x: hydra.errors.DecodingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.DecodingError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def empty_list_error(_: T0) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermUnit())

def multiple_bindings_error(x: hydra.errors.MultipleBindingsError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleBindingsError"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)),))))

def multiple_fields_error(x: hydra.errors.MultipleFieldsError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.MultipleFieldsError"), (hydra.core.Field(hydra.core.Name("fieldName"), hydra.encode.core.name(x.field_name)),))))

def no_matching_field_error(x: hydra.errors.NoMatchingFieldError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoMatchingFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), hydra.encode.core.name(x.field_name)),))))

def no_such_binding_error(x: hydra.errors.NoSuchBindingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchBindingError"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)),))))

def not_enough_cases_error(_: T0) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermUnit())

def unexpected_shape_error(x: hydra.errors.UnexpectedShapeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnexpectedShapeError"), (hydra.core.Field(hydra.core.Name("expected"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.expected))))), hydra.core.Field(hydra.core.Name("actual"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.actual)))))))))

def extraction_error(v1: hydra.errors.ExtractionError) -> hydra.core.Term:
    match v1:
        case hydra.errors.ExtractionErrorEmptyList(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("emptyList"), empty_list_error(y)))))

        case hydra.errors.ExtractionErrorMultipleBindings(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("multipleBindings"), multiple_bindings_error(y2)))))

        case hydra.errors.ExtractionErrorMultipleFields(value=y3):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("multipleFields"), multiple_fields_error(y3)))))

        case hydra.errors.ExtractionErrorNoMatchingField(value=y4):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("noMatchingField"), no_matching_field_error(y4)))))

        case hydra.errors.ExtractionErrorNoSuchBinding(value=y5):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("noSuchBinding"), no_such_binding_error(y5)))))

        case hydra.errors.ExtractionErrorNotEnoughCases(value=y6):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("notEnoughCases"), not_enough_cases_error(y6)))))

        case hydra.errors.ExtractionErrorUnexpectedShape(value=y7):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ExtractionError"), hydra.core.Field(hydra.core.Name("unexpectedShape"), unexpected_shape_error(y7)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def other_inference_error(x: hydra.errors.OtherInferenceError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.OtherInferenceError"), (hydra.core.Field(hydra.core.Name("path"), hydra.encode.paths.subterm_path(x.path)), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.message)))))))))

def unification_error(x: hydra.errors.UnificationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationError"), (hydra.core.Field(hydra.core.Name("leftType"), hydra.encode.core.type(x.left_type)), hydra.core.Field(hydra.core.Name("rightType"), hydra.encode.core.type(x.right_type)), hydra.core.Field(hydra.core.Name("message"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.message)))))))))

def unification_inference_error(x: hydra.errors.UnificationInferenceError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.UnificationInferenceError"), (hydra.core.Field(hydra.core.Name("path"), hydra.encode.paths.subterm_path(x.path)), hydra.core.Field(hydra.core.Name("cause"), unification_error(x.cause))))))

def inference_error(v1: hydra.errors.InferenceError) -> hydra.core.Term:
    match v1:
        case hydra.errors.InferenceErrorChecking(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("checking"), hydra.encode.error.checking.checking_error(y)))))

        case hydra.errors.InferenceErrorOther(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("other"), other_inference_error(y2)))))

        case hydra.errors.InferenceErrorUnification(value=y3):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.InferenceError"), hydra.core.Field(hydra.core.Name("unification"), unification_inference_error(y3)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def other_error(x: hydra.errors.OtherError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.OtherError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def no_such_primitive_error(x: hydra.errors.NoSuchPrimitiveError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.errors.NoSuchPrimitiveError"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)),))))

def other_resolution_error(x: hydra.errors.OtherResolutionError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermWrap(hydra.core.WrappedTerm(hydra.core.Name("hydra.errors.OtherResolutionError"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralString(x.value)))))))

def resolution_error(v1: hydra.errors.ResolutionError) -> hydra.core.Term:
    match v1:
        case hydra.errors.ResolutionErrorNoSuchBinding(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noSuchBinding"), no_such_binding_error(y)))))

        case hydra.errors.ResolutionErrorNoSuchPrimitive(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noSuchPrimitive"), no_such_primitive_error(y2)))))

        case hydra.errors.ResolutionErrorNoMatchingField(value=y3):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("noMatchingField"), no_matching_field_error(y3)))))

        case hydra.errors.ResolutionErrorOther(value=y4):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("other"), other_resolution_error(y4)))))

        case hydra.errors.ResolutionErrorUnexpectedShape(value=y5):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.ResolutionError"), hydra.core.Field(hydra.core.Name("unexpectedShape"), unexpected_shape_error(y5)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def error(v1: hydra.errors.Error) -> hydra.core.Term:
    match v1:
        case hydra.errors.ErrorChecking(value=y):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("checking"), hydra.encode.error.checking.checking_error(y)))))

        case hydra.errors.ErrorDecoding(value=y2):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("decoding"), decoding_error(y2)))))

        case hydra.errors.ErrorDuplicateBinding(value=y3):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateBinding"), hydra.encode.error.core.duplicate_binding_error(y3)))))

        case hydra.errors.ErrorDuplicateField(value=y4):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("duplicateField"), hydra.encode.error.core.duplicate_field_error(y4)))))

        case hydra.errors.ErrorExtraction(value=y5):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("extraction"), extraction_error(y5)))))

        case hydra.errors.ErrorInference(value=y6):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("inference"), inference_error(y6)))))

        case hydra.errors.ErrorOther(value=y7):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("other"), other_error(y7)))))

        case hydra.errors.ErrorResolution(value=y8):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("resolution"), resolution_error(y8)))))

        case hydra.errors.ErrorUndefinedField(value=y9):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedField"), hydra.encode.error.core.undefined_field_error(y9)))))

        case hydra.errors.ErrorUndefinedTermVariable(value=y10):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), hydra.encode.error.core.undefined_term_variable_error(y10)))))

        case hydra.errors.ErrorUntypedTermVariable(value=y11):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), hydra.encode.error.core.untyped_term_variable_error(y11)))))

        case hydra.errors.ErrorUnexpectedTermVariant(value=y12):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTermVariant"), hydra.encode.error.core.unexpected_term_variant_error(y12)))))

        case hydra.errors.ErrorUnexpectedTypeVariant(value=y13):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unexpectedTypeVariant"), hydra.encode.error.core.unexpected_type_variant_error(y13)))))

        case hydra.errors.ErrorUnification(value=y14):
            return cast(hydra.core.Term, hydra.core.TermInject(hydra.core.Injection(hydra.core.Name("hydra.errors.Error"), hydra.core.Field(hydra.core.Name("unification"), unification_error(y14)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")
