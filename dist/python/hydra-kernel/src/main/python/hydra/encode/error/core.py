# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.error.core."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.encode.core
import hydra.encode.paths
import hydra.encode.variants
import hydra.error.core

def constant_condition_error(x: hydra.error.core.ConstantConditionError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.ConstantConditionError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("value"), cast(hydra.core.Term, hydra.core.TermLiteral(cast(hydra.core.Literal, hydra.core.LiteralBoolean(x.value)))))))))

def duplicate_binding_error(x: hydra.error.core.DuplicateBindingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def duplicate_field_error(x: hydra.error.core.DuplicateFieldError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def duplicate_record_type_field_names_error(x: hydra.error.core.DuplicateRecordTypeFieldNamesError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def duplicate_union_type_field_names_error(x: hydra.error.core.DuplicateUnionTypeFieldNamesError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def empty_case_statement_error(x: hydra.error.core.EmptyCaseStatementError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyCaseStatementError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def empty_let_bindings_error(x: hydra.error.core.EmptyLetBindingsError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyLetBindingsError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def empty_record_type_error(x: hydra.error.core.EmptyRecordTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyRecordTypeError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def empty_term_annotation_error(x: hydra.error.core.EmptyTermAnnotationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def empty_type_annotation_error(x: hydra.error.core.EmptyTypeAnnotationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def empty_type_name_in_term_error(x: hydra.error.core.EmptyTypeNameInTermError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def empty_union_type_error(x: hydra.error.core.EmptyUnionTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.EmptyUnionTypeError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def invalid_forall_parameter_name_error(x: hydra.error.core.InvalidForallParameterNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidForallParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def invalid_lambda_parameter_name_error(x: hydra.error.core.InvalidLambdaParameterNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def invalid_let_binding_name_error(x: hydra.error.core.InvalidLetBindingNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidLetBindingNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def invalid_type_lambda_parameter_name_error(x: hydra.error.core.InvalidTypeLambdaParameterNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def nested_term_annotation_error(x: hydra.error.core.NestedTermAnnotationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTermAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def redundant_wrap_unwrap_error(x: hydra.error.core.RedundantWrapUnwrapError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def self_application_error(x: hydra.error.core.SelfApplicationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SelfApplicationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def term_variable_shadowing_error(x: hydra.error.core.TermVariableShadowingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TermVariableShadowingError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def type_variable_shadowing_in_type_lambda_error(x: hydra.error.core.TypeVariableShadowingInTypeLambdaError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def undefined_term_variable_error(x: hydra.error.core.UndefinedTermVariableError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def undefined_type_variable_in_binding_type_error(x: hydra.error.core.UndefinedTypeVariableInBindingTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def undefined_type_variable_in_lambda_domain_error(x: hydra.error.core.UndefinedTypeVariableInLambdaDomainError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def undefined_type_variable_in_type_application_error(x: hydra.error.core.UndefinedTypeVariableInTypeApplicationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def unknown_primitive_name_error(x: hydra.error.core.UnknownPrimitiveNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def unnecessary_identity_application_error(x: hydra.error.core.UnnecessaryIdentityApplicationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def untyped_term_variable_error(x: hydra.error.core.UntypedTermVariableError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UntypedTermVariableError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def invalid_term_error(v1: hydra.error.core.InvalidTermError) -> hydra.core.Term:
    match v1:
        case hydra.error.core.InvalidTermErrorConstantCondition(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("constantCondition"), constant_condition_error(y)))))

        case hydra.error.core.InvalidTermErrorDuplicateBinding(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateBinding"), duplicate_binding_error(y2)))))

        case hydra.error.core.InvalidTermErrorDuplicateField(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateField"), duplicate_field_error(y3)))))

        case hydra.error.core.InvalidTermErrorEmptyCaseStatement(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyCaseStatement"), empty_case_statement_error(y4)))))

        case hydra.error.core.InvalidTermErrorEmptyLetBindings(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyLetBindings"), empty_let_bindings_error(y5)))))

        case hydra.error.core.InvalidTermErrorEmptyTermAnnotation(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyTermAnnotation"), empty_term_annotation_error(y6)))))

        case hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("emptyTypeNameInTerm"), empty_type_name_in_term_error(y7)))))

        case hydra.error.core.InvalidTermErrorInvalidLambdaParameterName(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidLambdaParameterName"), invalid_lambda_parameter_name_error(y8)))))

        case hydra.error.core.InvalidTermErrorInvalidLetBindingName(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidLetBindingName"), invalid_let_binding_name_error(y9)))))

        case hydra.error.core.InvalidTermErrorInvalidTypeLambdaParameterName(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("invalidTypeLambdaParameterName"), invalid_type_lambda_parameter_name_error(y10)))))

        case hydra.error.core.InvalidTermErrorNestedTermAnnotation(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("nestedTermAnnotation"), nested_term_annotation_error(y11)))))

        case hydra.error.core.InvalidTermErrorRedundantWrapUnwrap(value=y12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("redundantWrapUnwrap"), redundant_wrap_unwrap_error(y12)))))

        case hydra.error.core.InvalidTermErrorSelfApplication(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("selfApplication"), self_application_error(y13)))))

        case hydra.error.core.InvalidTermErrorTermVariableShadowing(value=y14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("termVariableShadowing"), term_variable_shadowing_error(y14)))))

        case hydra.error.core.InvalidTermErrorTypeVariableShadowingInTypeLambda(value=y15):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("typeVariableShadowingInTypeLambda"), type_variable_shadowing_in_type_lambda_error(y15)))))

        case hydra.error.core.InvalidTermErrorUndefinedTermVariable(value=y16):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTermVariable"), undefined_term_variable_error(y16)))))

        case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInBindingType(value=y17):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInBindingType"), undefined_type_variable_in_binding_type_error(y17)))))

        case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInLambdaDomain(value=y18):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInLambdaDomain"), undefined_type_variable_in_lambda_domain_error(y18)))))

        case hydra.error.core.InvalidTermErrorUndefinedTypeVariableInTypeApplication(value=y19):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariableInTypeApplication"), undefined_type_variable_in_type_application_error(y19)))))

        case hydra.error.core.InvalidTermErrorUnknownPrimitiveName(value=y20):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("unknownPrimitiveName"), unknown_primitive_name_error(y20)))))

        case hydra.error.core.InvalidTermErrorUnnecessaryIdentityApplication(value=y21):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("unnecessaryIdentityApplication"), unnecessary_identity_application_error(y21)))))

        case hydra.error.core.InvalidTermErrorUntypedTermVariable(value=y22):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("untypedTermVariable"), untyped_term_variable_error(y22)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def invalid_type_scheme_variable_name_error(x: hydra.error.core.InvalidTypeSchemeVariableNameError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def nested_type_annotation_error(x: hydra.error.core.NestedTypeAnnotationError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NestedTypeAnnotationError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def non_comparable_map_key_type_error(x: hydra.error.core.NonComparableMapKeyTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("keyType"), hydra.encode.core.type(x.key_type))))))

def non_comparable_set_element_type_error(x: hydra.error.core.NonComparableSetElementTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("elementType"), hydra.encode.core.type(x.element_type))))))

def single_variant_union_error(x: hydra.error.core.SingleVariantUnionError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.SingleVariantUnionError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("fieldName"), hydra.encode.core.name(x.field_name))))))

def type_variable_shadowing_in_forall_error(x: hydra.error.core.TypeVariableShadowingInForallError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def undefined_type_variable_error(x: hydra.error.core.UndefinedTypeVariableError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeVariableError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def void_in_non_bottom_position_error(x: hydra.error.core.VoidInNonBottomPositionError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.paths.subterm_path(x.location)),))))

def invalid_type_error(v1: hydra.error.core.InvalidTypeError) -> hydra.core.Term:
    match v1:
        case hydra.error.core.InvalidTypeErrorDuplicateRecordTypeFieldNames(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("duplicateRecordTypeFieldNames"), duplicate_record_type_field_names_error(y)))))

        case hydra.error.core.InvalidTypeErrorDuplicateUnionTypeFieldNames(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("duplicateUnionTypeFieldNames"), duplicate_union_type_field_names_error(y2)))))

        case hydra.error.core.InvalidTypeErrorEmptyRecordType(value=y3):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyRecordType"), empty_record_type_error(y3)))))

        case hydra.error.core.InvalidTypeErrorEmptyTypeAnnotation(value=y4):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyTypeAnnotation"), empty_type_annotation_error(y4)))))

        case hydra.error.core.InvalidTypeErrorEmptyUnionType(value=y5):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("emptyUnionType"), empty_union_type_error(y5)))))

        case hydra.error.core.InvalidTypeErrorInvalidForallParameterName(value=y6):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("invalidForallParameterName"), invalid_forall_parameter_name_error(y6)))))

        case hydra.error.core.InvalidTypeErrorInvalidTypeSchemeVariableName(value=y7):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("invalidTypeSchemeVariableName"), invalid_type_scheme_variable_name_error(y7)))))

        case hydra.error.core.InvalidTypeErrorNestedTypeAnnotation(value=y8):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nestedTypeAnnotation"), nested_type_annotation_error(y8)))))

        case hydra.error.core.InvalidTypeErrorNonComparableMapKeyType(value=y9):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nonComparableMapKeyType"), non_comparable_map_key_type_error(y9)))))

        case hydra.error.core.InvalidTypeErrorNonComparableSetElementType(value=y10):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("nonComparableSetElementType"), non_comparable_set_element_type_error(y10)))))

        case hydra.error.core.InvalidTypeErrorSingleVariantUnion(value=y11):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("singleVariantUnion"), single_variant_union_error(y11)))))

        case hydra.error.core.InvalidTypeErrorTypeVariableShadowingInForall(value=y12):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("typeVariableShadowingInForall"), type_variable_shadowing_in_forall_error(y12)))))

        case hydra.error.core.InvalidTypeErrorUndefinedTypeVariable(value=y13):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("undefinedTypeVariable"), undefined_type_variable_error(y13)))))

        case hydra.error.core.InvalidTypeErrorVoidInNonBottomPosition(value=y14):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTypeError"), hydra.core.Field(hydra.core.Name("voidInNonBottomPosition"), void_in_non_bottom_position_error(y14)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def undefined_field_error(x: hydra.error.core.UndefinedFieldError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), hydra.encode.core.name(x.field_name)), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def unexpected_term_variant_error(x: hydra.error.core.UnexpectedTermVariantError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), hydra.encode.variants.term_variant(x.expected_variant)), hydra.core.Field(hydra.core.Name("actualTerm"), hydra.encode.core.term(x.actual_term))))))

def unexpected_type_variant_error(x: hydra.error.core.UnexpectedTypeVariantError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), hydra.encode.variants.type_variant(x.expected_variant)), hydra.core.Field(hydra.core.Name("actualType"), hydra.encode.core.type(x.actual_type))))))
